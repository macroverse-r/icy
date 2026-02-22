#' Read Local YAML Configuration (Internal)
#'
#' Low-level YAML reader for local configuration files. This is the shared
#' workhorse used by both get_config() and check_conflicts() to avoid circular
#' dependencies. It reads and parses the YAML file with no side effects.
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

  # Read and parse the YAML file
  tryCatch(
    {
      config_data <- yaml::read_yaml(config_file_path)

      # Extract requested section
      if (!section %in% names(config_data)) {
        .icy_stop(c(
          paste0("Section ", section, " not found in local config"),
          "i" = paste0("Available sections: ", paste(names(config_data), collapse = ", "))
        ))
      }

      config <- config_data[[section]]

      if (is.null(config) || length(config) == 0) {
        return(list())
      }

      # Resolve variable references and paths
      if (length(config) > 0) {
        config <- .resolve_variable_references(config)

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
            template_data <- yaml::read_yaml(template_files$fn_tmpl)
            if ("types" %in% names(template_data)) {
              template_data$types
            } else {
              list()
            }
          } else {
            list()
          }
        }, error = function(e) list())

        # Resolve paths for variables with type="path"
        for (var_name in names(config)) {
          if (!is.null(template_types[[var_name]]) && template_types[[var_name]] == "path") {
            if (is.character(config[[var_name]])) {
              config[[var_name]] <- .resolve_special_path(config[[var_name]], package, config)
            }
          }
        }
      }

      return(config)
    },
    error = function(e) {
      .icy_stop(paste0("Error reading YAML file: ", e$message))
    }
  )
}
