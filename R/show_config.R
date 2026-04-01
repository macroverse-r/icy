#' Show Configuration Status
#'
#' Displays the current configuration values from the config file
#' (single source of truth). When `show_template = TRUE`, shows template default
#' values alongside current values for comparison.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param var_names Optional character vector of specific variable names to show.
#'   If NULL (default), shows all variables defined in the configuration.
#' @param show_template Logical. If TRUE, shows template default values alongside
#'   current values. For non-path variables, defaults appear inline with
#'   match indicators. For path variables, defaults appear on an indented
#'   second line showing both the raw template keyword and its resolved path.
#'   Defaults to FALSE for clean output.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param name Optional character string for named configs (e.g., "gams_switches").
#'   If NULL (default), shows the main config file (\{package\}_config.yml).
#' @param confirm_fuzzy Logical. If TRUE (default), asks user to confirm fuzzy matches interactively.
#'
#' @return Invisibly returns a data frame with variable names and values.
#'   When `show_template = TRUE`, also includes `template_raw`, `type`, and
#'   `matches_template` columns.
#'
#' @examples
#' \dontrun{
#' # Show all configuration values (clean output)
#' show_config("mypackage")
#'
#' # Show with template comparison
#' show_config("mypackage", show_template = TRUE)
#'
#' # Show specific variables with template comparison
#' show_config("mypackage", var_names = c("API_KEY", "DB_HOST"), show_template = TRUE)
#' }
#'
#' @export
show_config <- function(package = get_package_name(),
                        var_names = NULL,
                        show_template = FALSE,
                        section = "default",
                        name = NULL,
                        confirm_fuzzy = TRUE) {

  # Resolve file paths once
  resolved_files <- .find_config_files(
    package = package,
    name = name,
    fuzzy = TRUE,
    confirm_fuzzy = confirm_fuzzy,
    verbose = FALSE
  )
  resolved_config_path <- resolved_files$fn_config
  resolved_template_path <- resolved_files$fn_tmpl

  # Read config (single source of truth)
  local_config <- tryCatch(
    {
      get_config(package = package,
                 section = section,
                 name = name,
                 confirm_fuzzy = FALSE)
    },
    error = function(e) list()
  )

  # Read raw template data when defaults mode is requested
  template_raw <- NULL
  template_types <- NULL

  if (show_template) {
    template_path <- resolved_template_path
    if (is.null(template_path)) {
      # Try finding the template directly
      tmpl_files <- .find_config_files(
        package = package,
        name = name,
        confirm_fuzzy = FALSE,
        verbose = FALSE
      )
      template_path <- tmpl_files$fn_tmpl
    }

    if (!is.null(template_path) && file.exists(template_path)) {
      template_data <- tryCatch(yaml::read_yaml(template_path), error = function(e) NULL)

      if (!is.null(template_data)) {
        if (section %in% names(template_data)) {
          template_raw <- template_data[[section]]
        }
        if ("types" %in% names(template_data)) {
          template_types <- template_data$types
        }
      }
    }
  }

  # Determine variable names to display
  if (is.null(var_names)) {
    if (show_template && !is.null(template_raw)) {
      var_names <- unique(c(names(template_raw), names(local_config)))
    } else {
      var_names <- names(local_config)
    }
  }

  if (length(var_names) == 0) {
    .icy_warn(paste0("No variables found in configuration for ", package))
    return(invisible(NULL))
  }

  # Display results
  for (var in var_names) {
    local_val <- local_config[[var]]

    colored_var <- .apply_color(var, "cyan")

    if (!is.null(local_val)) {
      colored_value <- .format_value_with_color(var, as.character(local_val))
    } else {
      colored_value <- .apply_color("(not set)", "grey")
    }

    if (!show_template || is.null(template_raw)) {
      # --- Simple mode: just VAR = value ---
      .icy_text(paste0(colored_var, " = ", colored_value))

    } else {
      # --- Defaults mode: show template comparison ---
      raw_default <- template_raw[[var]]
      var_type <- if (!is.null(template_types)) template_types[[var]] else NULL
      var_type <- .normalize_type(var_type)
      is_path <- (!is.null(var_type) && var_type == "path")

      if (is.null(raw_default) && !var %in% names(template_raw)) {
        # Variable not in template (user-added)
        .icy_text(paste0(colored_var, " = ", colored_value,
                          .apply_color(" [not in template]", "gray")))

      } else if (is_path) {
        # --- Path variable: two-line display ---
        .icy_text(paste0(colored_var, " = ", colored_value))

        raw_str <- .format_raw_default(raw_default)
        resolved_default <- .resolve_template_path_for_display(
          raw_default, local_config, package
        )

        if (is.null(resolved_default) || raw_str == resolved_default) {
          default_display <- .apply_color(
            paste0("[tmpl: ", raw_str, "]"), "gray"
          )
        } else {
          default_display <- .apply_color(
            paste0("[tmpl: ", raw_str, " \u2192 ", resolved_default, "]"), "gray"
          )
        }
        # Use cat() directly to preserve leading indentation (strwrap strips it)
        cat("  ", default_display, "\n", sep = "")

      } else {
        # --- Non-path variable: inline display ---
        raw_str <- .format_raw_default(raw_default)

        if (!is.null(local_val)) {
          if (is.null(raw_default)) {
            # Default is NULL/~, local has a value
            match_symbol <- .apply_color(" \u2717", "red")
          } else {
            matches <- (as.character(local_val) == as.character(raw_default))
            match_symbol <- if (matches) {
              .apply_color(" \u2713", "green")
            } else {
              .apply_color(" \u2717", "red")
            }
          }
          default_info <- paste0(
            .apply_color(paste0(" [tmpl: ", raw_str, "]"), "gray"),
            match_symbol
          )
        } else if (is.null(raw_default)) {
          # Both null -- match
          default_info <- paste0(
            .apply_color(" [tmpl: ~]", "gray"),
            .apply_color(" \u2713", "green")
          )
        } else {
          # Local not set, template has value
          default_info <- .apply_color(paste0(" [tmpl: ", raw_str, "]"), "gray")
        }

        .icy_text(paste0(colored_var, " = ", colored_value, default_info))
      }
    }
  }

  # Build return data frame
  status_df <- data.frame(
    variable = var_names,
    value = vapply(var_names, function(v) {
      val <- local_config[[v]]
      if (is.null(val)) "(not set)" else as.character(val)
    }, character(1)),
    stringsAsFactors = FALSE
  )

  if (show_template && !is.null(template_raw)) {
    status_df$template_raw <- vapply(var_names, function(v) {
      val <- template_raw[[v]]
      if (is.null(val)) "~" else as.character(val)
    }, character(1))

    status_df$type <- vapply(var_names, function(v) {
      tp <- if (!is.null(template_types)) template_types[[v]] else NA_character_
      if (is.null(tp)) NA_character_ else as.character(tp)
    }, character(1))

    status_df$matches_template <- vapply(var_names, function(v) {
      local_val <- local_config[[v]]
      raw_val <- template_raw[[v]]
      if (is.null(local_val) && is.null(raw_val)) return(TRUE)
      if (is.null(local_val) || is.null(raw_val)) return(FALSE)
      as.character(local_val) == as.character(raw_val)
    }, logical(1))
  }

  return(invisible(status_df))
}


#' Format raw template default value for display
#' @keywords internal
.format_raw_default <- function(value) {
  if (is.null(value)) return("~")
  if (is.logical(value)) return(toupper(as.character(value)))
  return(as.character(value))
}


#' Resolve template path for display illustration
#'
#' Substitutes variable references using config values,
#' then resolves keywords for the display arrow.
#'
#' @keywords internal
.resolve_template_path_for_display <- function(raw_value, local_config, package) {
  if (is.null(raw_value) || !is.character(raw_value)) return(NULL)

  resolved <- raw_value

  # Substitute ${VAR_NAME} references using config values
  pattern <- "\\$\\{([A-Z_][A-Z0-9_]*)\\}"
  matches <- gregexpr(pattern, resolved, perl = TRUE)

  if (matches[[1]][1] != -1) {
    match_starts <- matches[[1]]
    match_lengths <- attr(matches[[1]], "match.length")
    capture_starts <- attr(matches[[1]], "capture.start")
    capture_lengths <- attr(matches[[1]], "capture.length")

    # Replace from right to left to preserve positions
    for (i in length(match_starts):1) {
      full_match <- substr(resolved, match_starts[i],
                          match_starts[i] + match_lengths[i] - 1)
      ref_var <- substr(resolved, capture_starts[i],
                       capture_starts[i] + capture_lengths[i] - 1)

      if (ref_var %in% names(local_config)) {
        resolved <- sub(full_match, as.character(local_config[[ref_var]]),
                       resolved, fixed = TRUE)
      }
    }
  }

  # Resolve keywords (getwd, tempdir, home, etc.)
  resolved <- .resolve_special_path(resolved, package, local_config)

  return(resolved)
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
    return(.apply_color(value_str, "green"))
  } else if (value_str %in% c("TRUE", "FALSE", "true", "false", "yes", "no")) {
    return(.apply_color(value_str, "orange"))
  } else if (grepl("^[0-9]+$", value_str)) {
    return(.apply_color(value_str, "magenta"))
  } else {
    return(.apply_color(paste0("'", value_str, "'"), "brown"))
  }
}
