#' Check for Configuration Conflicts
#'
#' Detects conflicts between configuration values and R session environment
#' variables. When a variable exists in both the config and the session
#' environment with different values, this indicates ambiguity about the true
#' configuration state.
#'
#' In "resolve" mode (default when called directly), the function interactively
#' walks the user through each conflict, asking them to choose which value to
#' keep. If the session value has an invalid type (e.g., "hello" for a boolean),
#' the conflict is auto-resolved by keeping the config value.
#' The session variable is always cleaned up, and if the conflict originates
#' from ~/.Renviron, the user is offered to remove it there too.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()`.
#' @param mode Character string controlling conflict handling behavior:
#'   - "resolve" (default): Interactive resolution of each conflict
#'   - "warn": Warn about conflicts, suggest check_conflicts()
#'   - "silent": No warnings, no interaction
#' @param config Named list of config values. If NULL, reads from config
#'   file. Used internally by get_config() to avoid re-reading.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param verbose Logical. If TRUE, shows detailed messages. Defaults to FALSE.
#'
#' @return Invisibly returns a list with:
#'   \describe{
#'     \item{conflicts}{Named list of detected conflicts (var_name -> list(config, session, from_renviron))}
#'     \item{resolved}{Logical indicating whether conflicts were resolved}
#'   }
#'
#' @examples
#' \dontrun{
#' # Interactive conflict resolution (default)
#' check_conflicts(package = "mypackage")
#'
#' # Just check for conflicts without resolving
#' result <- check_conflicts(package = "mypackage", mode = "warn")
#' }
#'
#' @export
check_conflicts <- function(package = get_package_name(),
                            mode = "resolve",
                            config = NULL,
                            section = "default",
                            verbose = FALSE) {

  # Validate mode
  valid_modes <- c("resolve", "warn", "silent")
  if (!mode %in% valid_modes) {
    .icy_stop(c(
      paste0("Invalid mode: ", mode),
      "i" = paste0("Valid modes are: ", paste(valid_modes, collapse = ", "))
    ))
  }

  # Get config values (from argument or by reading)
  if (is.null(config)) {
    config_files <- .find_config_files(package = package)
    raw_data <- if (!is.null(config_files$fn_config)) {
      tryCatch(yaml::read_yaml(config_files$fn_config), error = function(e) NULL)
    }
    config <- .process_config_section(raw_data, section = section,
                                      source_label = "config")
  }

  if (length(config) == 0) {
    return(invisible(list(conflicts = list(), resolved = FALSE)))
  }

  # Detect conflicts: vectorized lookup, loop only over overlapping vars
  cfg_names <- names(config)
  session_vals <- Sys.getenv(cfg_names, unset = NA_character_)
  present <- cfg_names[!is.na(session_vals)]

  conflicts <- list()
  for (var_name in present) {
    config_value <- config[[var_name]]
    session_value <- session_vals[[var_name]]
    if (is.null(config_value)) {
      conflicts[[var_name]] <- list(
        config = "(not set)",
        session = session_value,
        from_renviron = FALSE
      )
    } else {
      config_value <- as.character(config_value)
      if (tolower(session_value) != tolower(config_value)) {
        conflicts[[var_name]] <- list(
          config = config_value,
          session = session_value,
          from_renviron = FALSE
        )
      }
    }
  }

  if (length(conflicts) == 0) {
    if (verbose) {
      .icy_success("No conflicts detected between config and session environment")
    }
    return(invisible(list(conflicts = list(), resolved = FALSE)))
  }

  # Handle conflicts based on mode
  if (mode == "silent") {
    return(invisible(list(conflicts = conflicts, resolved = FALSE)))
  }

  if (mode == "warn") {
    conflict_names <- names(conflicts)
    .icy_warn(paste0(
      "Found ", length(conflicts), " conflict",
      if (length(conflicts) > 1) "s" else "",
      " between config and session environment: ",
      paste(conflict_names, collapse = ", "),
      ". Run check_conflicts() to resolve interactively."
    ))
    return(invisible(list(conflicts = conflicts, resolved = FALSE)))
  }

  # Check which conflicts originate from .Renviron (only needed for resolve mode)
  renviron_vars <- .parse_renviron_vars()
  for (var_name in names(conflicts)) {
    if (var_name %in% names(renviron_vars) && renviron_vars[[var_name]] == conflicts[[var_name]]$session) {
      conflicts[[var_name]]$from_renviron <- TRUE
    }
  }

  # mode == "resolve": Interactive resolution
  # Read template types for validation
  template_types <- tryCatch({
    tmpl_files <- .find_config_files(package = package, verbose = FALSE)
    if (!is.null(tmpl_files$fn_tmpl)) {
      tmpl_data <- yaml::read_yaml(tmpl_files$fn_tmpl)
      if ("types" %in% names(tmpl_data)) tmpl_data$types else list()
    } else list()
  }, error = function(e) list())

  .icy_inform(paste0("Found ", length(conflicts), " configuration conflict",
                     if (length(conflicts) > 1) "s" else ""))

  skipped <- 0L
  for (var_name in names(conflicts)) {
    conflict <- conflicts[[var_name]]

    # Check if session value has a valid type
    var_type <- .normalize_type(template_types[[var_name]])
    type_check <- .validate_variable_type(conflict$session, var_type, var_name)

    .icy_title(paste0("Conflict: ", var_name))
    .icy_text(paste0("  Config value: ", .apply_color(conflict$config, color = "green")))

    # Show session value with inline type warning if invalid
    session_display <- .apply_color(conflict$session, color = "yellow")
    if (conflict$from_renviron) {
      session_display <- paste0(session_display, .apply_color(" (from .Renviron)", color = "gray"))
    }
    if (!type_check$valid) {
      session_display <- paste0(session_display,
        .apply_color(paste0(" (invalid: expected ", var_type, ")"), color = "red"))
    }
    .icy_text(paste0("  Session value:      ", session_display))

    if (!type_check$valid) {
      options <- c(
        paste0("Keep config value (", conflict$config, ")"),
        "Skip (leave conflict unresolved)"
      )
      .icy_text(.apply_color("Select which value to keep:", color = "brown"))
      .icy_bullets(options, bullet = "1:")
      .icy_text(paste0("Enter your choice: ",
                       .apply_color("(1-2, or press Enter to keep config)", color = "gray")))
    } else {
      options <- c(
        paste0("Keep config value (", conflict$config, ")"),
        paste0("Use session value (", conflict$session, ") and update config"),
        "Skip (leave conflict unresolved)"
      )
      .icy_text(.apply_color("Select which value to keep:", color = "brown"))
      .icy_bullets(options, bullet = "1:")
      .icy_text(paste0("Enter your choice: ",
                       .apply_color("(1-3, or press Enter to keep config)", color = "gray")))
    }

    choice <- readline()
    choice <- trimws(choice)

    # Determine skip option number (2 for invalid type, 3 for valid)
    skip_num <- if (!type_check$valid) "2" else "3"
    use_session_num <- if (type_check$valid) "2" else NULL

    if (choice == skip_num) {
      skipped <- skipped + 1L
      .icy_inform(paste0("Skipped: ", var_name, " conflict left unresolved"))
      .icy_text("")
      next
    }

    if (!is.null(use_session_num) && choice == use_session_num) {
      # Use session value: update config
      var_list <- structure(list(conflict$session), names = var_name)
      update_config(
        var_list = var_list,
        package = package,
        section = section,
        sync = "none",
        verbose = FALSE
      )
      .icy_success(paste0("Updated config: ", var_name, " = ", conflict$session))
    } else {
      # Default: keep config (choice "1" or Enter)
      .icy_success(paste0("Keeping config value: ", var_name, " = ", conflict$config))
    }

    # Clean the session variable
    Sys.unsetenv(var_name)
    if (verbose) {
      .icy_text(paste0("  Removed ", var_name, " from session environment"))
    }

    # If from .Renviron, offer to remove
    if (conflict$from_renviron) {
      .icy_text("")
      .icy_text(paste0("This variable is also defined in ",
                       .apply_color("~/.Renviron", color = "yellow")))
      .icy_text(paste0("Remove ", var_name, " from .Renviron? ",
                       .apply_color("(Y/n)", color = "gray")))

      remove_choice <- readline()
      remove_choice <- trimws(tolower(remove_choice))

      if (remove_choice == "" || remove_choice == "y" || remove_choice == "yes") {
        .remove_from_renviron(var_name)
        .icy_success(paste0("Removed ", var_name, " from .Renviron"))
      }
    }

    .icy_text("")
  }

  if (skipped == 0L) {
    .icy_success("All conflicts resolved. Local config is the single source of truth.")
  } else if (skipped == length(conflicts)) {
    .icy_inform(paste0("All ", skipped, " conflict", if (skipped > 1) "s" else "", " skipped."))
  } else {
    resolved <- length(conflicts) - skipped
    .icy_success(paste0(resolved, " conflict", if (resolved > 1) "s" else "", " resolved, ",
                        skipped, " skipped."))
  }

  return(invisible(list(conflicts = conflicts, resolved = (skipped < length(conflicts)))))
}


#' Parse .Renviron file for variable definitions
#'
#' @return Named list of variable name -> value pairs from .Renviron
#' @keywords internal
.parse_renviron_vars <- function() {
  renviron_path <- path.expand("~/.Renviron")

  if (!file.exists(renviron_path)) {
    return(list())
  }

  lines <- readLines(renviron_path, warn = FALSE)
  env_vars <- list()

  for (line in lines) {
    if (nchar(trimws(line)) == 0 || grepl("^\\s*#", line)) {
      next
    }

    if (grepl("^\\s*[A-Za-z_][A-Za-z0-9_]*\\s*=", line)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        var_name <- trimws(parts[1])
        var_value <- paste(parts[-1], collapse = "=")
        var_value <- trimws(var_value)

        if (grepl('^".*"$', var_value) || grepl("^'.*'$", var_value)) {
          var_value <- substr(var_value, 2, nchar(var_value) - 1)
        }

        env_vars[[var_name]] <- var_value
      }
    }
  }

  return(env_vars)
}


#' Remove a variable from .Renviron file
#'
#' @param var_name Character string with the variable name to remove
#' @keywords internal
.remove_from_renviron <- function(var_name) {
  renviron_path <- path.expand("~/.Renviron")

  if (!file.exists(renviron_path)) {
    return(invisible(NULL))
  }

  lines <- readLines(renviron_path, warn = FALSE)

  # Filter out lines that define this variable
  pattern <- paste0("^\\s*", var_name, "\\s*=")
  filtered_lines <- lines[!grepl(pattern, lines)]

  # Remove trailing blank lines
  while (length(filtered_lines) > 0 && trimws(filtered_lines[length(filtered_lines)]) == "") {
    filtered_lines <- filtered_lines[-length(filtered_lines)]
  }

  writeLines(filtered_lines, renviron_path)
  return(invisible(NULL))
}
