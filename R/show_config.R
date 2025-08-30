#' Show Environment Variable Status
#'
#' Displays the current values and status of environment variables, including
#' information about where each value comes from (e.g., .Renviron, local config,
#' or not set). This function provides comprehensive visibility into the
#' configuration state.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param var_names Optional character vector of specific variable names to show.
#'   If NULL (default), shows all variables defined in the configuration.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param display Character string specifying display mode:
#'   - "sources": Show variable names, values, and source information (default)
#'   - "values": Show only variable names and values (no source info)
#'   - "full": Show detailed information about all configuration sources
#' @param fn_tmpl Character string with the name or path to a custom YAML template file.
#'   If NULL (default), uses the standard template file for the package.
#' @param fn_local Character string with the name or path to a custom local YAML config file.
#'   If NULL (default), uses the standard local config file for the package.
#' @param confirm_fuzzy Logical. If TRUE (default), asks user to confirm fuzzy matches interactively.
#'
#' @return Invisibly returns a data frame with variable names, values, and sources.
#'
#' @examples
#' \dontrun{
#' # Show all environment variables with sources
#' show_config("mypackage")
#'
#' # Show specific variables only
#' show_config("mypackage", var_names = c("API_KEY", "DB_HOST"))
#'
#' # Show only values without source information
#' show_config("mypackage", display = "values")
#'
#' # Show full detailed information
#' show_config("mypackage", display = "full")
#' }
#'
#' @export
show_config <- function(package = get_package_name(),
                        var_names = NULL,
                        section = "default",
                        display = "sources",
                        fn_tmpl = NULL,
                        fn_local = NULL,
                        confirm_fuzzy = TRUE) {

  # Resolve file paths once at the beginning to avoid multiple confirmations
  resolved_local_path <- NULL
  resolved_template_path <- NULL
  
  if (!is.null(fn_tmpl) || !is.null(fn_local)) {
    resolved_files <- find_config_files(
      package = package,
      fn_local = fn_local,
      fn_tmpl = fn_tmpl,
      fuzzy = TRUE,
      confirm_fuzzy = confirm_fuzzy,
      verbose = FALSE
    )
    resolved_local_path <- resolved_files$fn_local
    resolved_template_path <- resolved_files$fn_tmpl
  }

  # Get all possible variable names if not specified
  if (is.null(var_names)) {
    # Try to get from template to see all possible variables
    template_vars <- tryCatch(
      {
        names(get_config(package = package,
                         origin = "template",
                         section = section,
                         fn_tmpl = if (is.null(resolved_template_path)) fn_tmpl else basename(resolved_template_path),
                         fn_local = if (is.null(resolved_local_path)) fn_local else basename(resolved_local_path),
                         confirm_fuzzy = FALSE))
      },
      error = function(e) NULL
    )

    # Also get from local config
    local_vars <- tryCatch(
      {
        names(get_config(package = package,
                         origin = "local",
                         section = section,
                         fn_tmpl = if (is.null(resolved_template_path)) fn_tmpl else basename(resolved_template_path),
                         fn_local = if (is.null(resolved_local_path)) fn_local else basename(resolved_local_path),
                         confirm_fuzzy = FALSE))
      },
      error = function(e) NULL
    )

    var_names <- unique(c(template_vars, local_vars))

    if (length(var_names) == 0) {
      .icy_warn(paste0("No variables found in configuration for ", package))
      return(invisible(NULL))
    }
  }
  

  # Build status information
  status_df <- data.frame(
    variable = var_names,
    value = character(length(var_names)),
    source = character(length(var_names)),
    stringsAsFactors = FALSE
  )

  # Get values from different sources
  renviron_config <- tryCatch(
    {
      get_config(package = package, 
                 origin = "renviron", 
                 section = section,
                 fn_tmpl = if (is.null(resolved_template_path)) fn_tmpl else basename(resolved_template_path),
                 fn_local = if (is.null(resolved_local_path)) fn_local else basename(resolved_local_path),
                 confirm_fuzzy = FALSE)
    },
    error = function(e) list()
  )

  local_config <- tryCatch(
    {
      get_config(package = package, 
                 origin = "local", 
                 section = section,
                 fn_tmpl = if (is.null(resolved_template_path)) fn_tmpl else basename(resolved_template_path),
                 fn_local = if (is.null(resolved_local_path)) fn_local else basename(resolved_local_path),
                 confirm_fuzzy = FALSE)
    },
    error = function(e) list()
  )

  # Check each variable
  for (i in seq_along(var_names)) {
    var <- var_names[i]

    # Check current session
    session_value <- Sys.getenv(var, unset = NA)

    # Determine value and source with priority: session > .Renviron > local config
    if (!is.na(session_value)) {
      status_df$value[i] <- session_value
      # Determine if session value matches a config source
      if (var %in% names(renviron_config) && !is.null(renviron_config[[var]]) && !is.na(renviron_config[[var]]) && renviron_config[[var]] == session_value) {
        status_df$source[i] <- "session = .Renviron"
      } else if (var %in% names(local_config) && !is.null(local_config[[var]]) && !is.na(local_config[[var]]) && local_config[[var]] == session_value) {
        status_df$source[i] <- "session = local"
      } else {
        status_df$source[i] <- "session"
      }
    } else if (var %in% names(renviron_config) && !is.null(renviron_config[[var]])) {
      status_df$value[i] <- as.character(renviron_config[[var]])
      status_df$source[i] <- ".Renviron"
    } else if (var %in% names(local_config) && !is.null(local_config[[var]])) {
      status_df$value[i] <- as.character(local_config[[var]])
      status_df$source[i] <- "local config"
    } else {
      status_df$value[i] <- "(not set)"
      status_df$source[i] <- "not set"
    }
  }

  # Validate display mode
  valid_modes <- c("sources", "values", "full")
  if (!display %in% valid_modes) {
    .icy_stop(c(
      paste0("Invalid display mode: ", display),
      "i" = paste0("Valid modes are: ", paste(valid_modes, collapse = ", "))
    ))
  }

  # Display results based on mode
  if (display == "full") {
    .show_config_full(package, status_df, renviron_config, local_config, section, 
                     fn_tmpl = if (is.null(resolved_template_path)) fn_tmpl else basename(resolved_template_path),
                     fn_local = if (is.null(resolved_local_path)) fn_local else basename(resolved_local_path))
  } else {
    .show_config_standard(package, status_df, display)
  }

  return(invisible(status_df))
}


#' Standard display mode for show_config
#' @keywords internal
.show_config_standard <- function(package, status_df, display) {
  for (i in seq_len(nrow(status_df))) {
    var <- status_df$variable[i]
    value <- status_df$value[i]
    source <- status_df$source[i]

    # Color-code the variable name
    colored_var <- .apply_color(var, "cyan")
    
    # Color-code the value using consistent helper function
    colored_value <- .format_value_with_color(var, value)

    # Format source information
    if (display == "sources" && !is.na(value) && value != "(not set)") {
      source_color <- switch(source,
        ".Renviron" = "magenta",
        "local config" = "blue", 
        "session" = "yellow",
        "session = .Renviron" = "yellow",
        "session = local" = "yellow",
        "not set" = "gray",
        "cyan"  # default
      )
      colored_source <- paste0(" [", .apply_color(source, source_color), "]")
      .icy_text(paste0(colored_var, " = ", colored_value, colored_source))
    } else {
      .icy_text(paste0(colored_var, " = ", colored_value))
    }
  }
}


#' Full display mode for show_config
#' @keywords internal
.show_config_full <- function(package, status_df, renviron_config, local_config, section, fn_tmpl = NULL, fn_local = NULL) {
  # Show template configuration
  template_config <- tryCatch({
    get_config(package = package, origin = "template", section = section, fn_tmpl = fn_tmpl, fn_local = fn_local, confirm_fuzzy = FALSE)
  }, error = function(e) list())
  
  if (length(template_config) > 0) {
    .icy_text(.apply_color("Template Configuration:", "green", "bold"))
    for (var in names(template_config)) {
      value <- template_config[[var]]
      colored_value <- .format_value_with_color(var, value, "(null)")
      .icy_text(paste0("  ", .apply_color(var, "cyan"), " = ", colored_value))
    }
    .icy_text("")
  }
  
  # Show local configuration
  if (length(local_config) > 0) {
    .icy_text(.apply_color("Local Configuration:", "magenta", "bold"))
    for (var in names(local_config)) {
      value <- local_config[[var]]
      colored_value <- .format_value_with_color(var, value, "(null)")
      .icy_text(paste0("  ", .apply_color(var, "cyan"), " = ", colored_value))
    }
    .icy_text("")
  }
  
  # Show .Renviron configuration
  if (length(renviron_config) > 0) {
    .icy_text(.apply_color(".Renviron Configuration:", "red", "bold"))
    for (var in names(renviron_config)) {
      value <- renviron_config[[var]]
      colored_value <- .format_value_with_color(var, value)
      .icy_text(paste0("  ", .apply_color(var, "cyan"), " = ", colored_value))
    }
    .icy_text("")
  }
  
  # Show session environment variables
  session_vars <- list()
  if (length(template_config) > 0) {
    for (var in names(template_config)) {
      session_value <- Sys.getenv(var, unset = NA)
      if (!is.na(session_value)) {
        session_vars[[var]] <- session_value
      }
    }
  }
  
  if (length(session_vars) > 0) {
    .icy_text(.apply_color("Session Environment:", "yellow", "bold"))
    for (var in names(session_vars)) {
      value <- session_vars[[var]]
      colored_value <- .format_value_with_color(var, value)
      .icy_text(paste0("  ", .apply_color(var, "cyan"), " = ", colored_value))
    }
    .icy_text("")
  }
  
  # Show final resolved values
  .icy_text(.apply_color("ICY Final Values (Priority: Session > .Renviron > Local > Template):", "blue", "bold"))
  for (i in seq_len(nrow(status_df))) {
    var <- status_df$variable[i]
    value <- status_df$value[i] 
    source <- status_df$source[i]
    
    # Use consistent value coloring
    colored_value <- .format_value_with_color(var, value)
    
    source_color <- switch(source,
      ".Renviron" = "magenta",
      "local config" = "blue",
      "session" = "yellow",
      "session = .Renviron" = "yellow",
      "session = local" = "yellow",
      "not set" = "gray",
      "cyan"
    )
    
    .icy_text(paste0("  ", .apply_color(var, "cyan"), " = ", colored_value, 
                    " [", .apply_color(source, source_color), "]"))
  }
}


#' Format value with consistent color coding
#' @keywords internal
.format_value_with_color <- function(var, value, null_replacement = "(not set)") {
  if (is.null(value)) {
    return(.apply_color(null_replacement, "grey"))
  }
  
  value_str <- as.character(value)
  
  if (is.na(value_str) || value_str == null_replacement) {
    return(.apply_color(if(is.na(value_str)) null_replacement else value_str, "grey"))
  } else if (grepl("_DIR$|_PATH$", var)) {
    # File paths in green
    return(.apply_color(value_str, "green"))
  } else if (value_str %in% c("TRUE", "FALSE", "true", "false", "yes", "no")) {
    # Boolean values in orange (256-color)
    return(.apply_color(value_str, "orange"))
  } else if (grepl("^[0-9]+$", value_str)) {
    # Numeric values in magenta
    return(.apply_color(value_str, "magenta"))
  } else {
    # Regular text values with quotes in brown
    return(.apply_color(paste0("'", value_str, "'"), "brown"))
  }
}
