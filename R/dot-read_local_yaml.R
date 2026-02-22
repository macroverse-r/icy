#' Resolve a YAML Section with Variable and Path Resolution
#'
#' Shared helper that extracts a section from parsed YAML data, resolves
#' variable references, and resolves path variables using template types.
#' Used by both .read_local_yaml() and .get_config_template().
#'
#' @param config_data Parsed YAML data (from yaml::read_yaml)
#' @param section Section name to extract
#' @param source_label Label for error messages (e.g., "local config", "template")
#' @param template_types Named list of variable types from template, or NULL
#' @param package Package name for path resolution
#' @return Named list of resolved configuration values
#' @keywords internal
._resolve_config_section <- function(config_data, section, source_label,
                                      template_types = NULL, package) {
  if (!section %in% names(config_data)) {
    .icy_stop(c(
      paste0("Section ", section, " not found in ", source_label),
      "i" = paste0("Available sections: ", paste(names(config_data), collapse = ", "))
    ))
  }

  config <- config_data[[section]]

  if (is.null(config) || length(config) == 0) {
    return(list())
  }

  config <- .resolve_variable_references(config)

  if (!is.null(template_types)) {
    for (var_name in names(config)) {
      if (!is.null(template_types[[var_name]]) && template_types[[var_name]] == "path") {
        if (is.character(config[[var_name]])) {
          config[[var_name]] <- .resolve_special_path(config[[var_name]], package, config)
        }
      }
    }
  }

  return(config)
}


#' Read Local YAML Configuration (Internal)
#'
#' Low-level YAML reader for local configuration files. Used by get_config()
#' as its local config reader and by check_conflicts() for standalone config
#' reading. Reads and parses the YAML file with no side effects.
#'
#' @param package Character string with the package name.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param resolved_local_path Pre-resolved path to the local config file. If NULL,
#'   the function will search for it using .find_config_files().
#' @param case_format Character string for filename generation.
#' @param verbose Logical. If TRUE, shows messages.
#'
#' @return Named list of configuration values from the local config file.
#'   Returns empty list if local config file does not exist.
#' @keywords internal
.read_local_yaml <- function(package,
                             section = "default",
                             resolved_local_path = NULL,
                             case_format = "snake_case",
                             verbose = FALSE) {

  # Locate the local config file
  if (is.null(resolved_local_path)) {
    config_files <- .find_config_files(
      package = package,
      case_format = case_format,
      verbose = verbose
    )
    config_file_path <- config_files$fn_local

    if (is.null(config_file_path)) {
      return(list())
    }
  } else {
    config_file_path <- resolved_local_path
  }

  if (verbose) {
    .icy_text(paste0("Reading local config from: ", config_file_path))
  }

  tryCatch(
    {
      config_data <- yaml::read_yaml(config_file_path)

      # Get types from template for path resolution
      template_types <- tryCatch({
        template_files <- .find_config_files(
          package = package,
          fn_tmpl = NULL,
          fn_local = NULL,
          case_format = case_format,
          verbose = FALSE,
          confirm_fuzzy = FALSE
        )
        if (!is.null(template_files$fn_tmpl) && file.exists(template_files$fn_tmpl)) {
          tmpl_data <- yaml::read_yaml(template_files$fn_tmpl)
          if ("types" %in% names(tmpl_data)) tmpl_data$types else list()
        } else {
          list()
        }
      }, error = function(e) list())

      ._resolve_config_section(config_data, section, "local config",
                                template_types, package)
    },
    error = function(e) {
      .icy_stop(paste0("Error reading YAML file: ", e$message))
    }
  )
}
