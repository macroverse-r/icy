#' Read YAML Configuration (Internal)
#'
#' Unified low-level YAML reader for both local and template configuration files.
#' Reads the file, extracts a section, resolves variable references, and resolves
#' path variables using template types.
#'
#' Used by get_config() as its internal reader, by check_conflicts() for standalone
#' config reading, by setup() for template inspection, and by .get_template_value()
#' for template metadata access.
#'
#' @param package Character string with the package name.
#' @param type Character string: "local" or "template".
#' @param section Character string for the section in the YAML file (default: "default").
#' @param resolved_path Pre-resolved path to the config file. If NULL,
#'   the function will search for it using .find_config_files().
#' @param case_format Character string for filename generation.
#' @param verbose Logical. If TRUE, shows messages.
#' @param resolved_template_path Pre-resolved path to the template file.
#'   Used for template type lookup when reading local configs.
#' @param config_data Pre-parsed YAML data. When provided, skips reading from disk.
#' @param template_types Pre-extracted template types list. When provided,
#'   skips template file lookup for type resolution.
#'
#' @return Named list of configuration values.
#'   For type "local", returns empty list if file does not exist.
#'   For type "template", errors if file does not exist.
#' @keywords internal
.read_yaml_config <- function(package,
                              type = c("local", "template"),
                              section = "default",
                              resolved_path = NULL,
                              case_format = "snake_case",
                              verbose = FALSE,
                              resolved_template_path = NULL,
                              config_data = NULL,
                              template_types = NULL) {
  type <- match.arg(type)

  # Find file if not provided (only needed when config_data is NULL)
  if (is.null(config_data) && is.null(resolved_path)) {
    config_files <- .find_config_files(
      package = package,
      case_format = case_format,
      verbose = if (type == "local") verbose else FALSE
    )
    config_file_path <- if (type == "template") config_files$fn_tmpl else config_files$fn_local

    if (is.null(config_file_path)) {
      if (type == "template") {
        .icy_stop(paste0("No template configuration file found for package ", package))
      }
      return(list())
    }
  } else {
    config_file_path <- resolved_path
  }

  if (verbose && is.null(config_data)) {
    label <- if (type == "template") "template" else "local"
    .icy_text(paste0("Reading ", label, " config from: ", config_file_path))
  }

  source_label <- if (type == "template") "template" else "local config"

  tryCatch(
    {
      # Use pre-parsed data or read from file
      if (is.null(config_data)) {
        config_data <- yaml::read_yaml(config_file_path)
      }

      # Get template types for path resolution (skip if pre-provided)
      if (is.null(template_types)) {
        template_types <- if (type == "template") {
          if ("types" %in% names(config_data)) config_data$types else NULL
        } else {
          # Use pre-resolved template path if provided, otherwise search
          tryCatch({
            tmpl_path <- resolved_template_path
            if (is.null(tmpl_path)) {
              template_files <- .find_config_files(
                package = package,
                case_format = case_format,
                verbose = FALSE,
                confirm_fuzzy = FALSE
              )
              tmpl_path <- template_files$fn_tmpl
            }
            if (!is.null(tmpl_path) && file.exists(tmpl_path)) {
              tmpl_data <- yaml::read_yaml(tmpl_path)
              if ("types" %in% names(tmpl_data)) tmpl_data$types else NULL
            } else {
              NULL
            }
          }, error = function(e) NULL)
        }
      }

      # Extract section, resolve variable references and paths
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
    },
    error = function(e) {
      error_label <- if (type == "template") "template YAML file" else "YAML file"
      .icy_stop(paste0("Error reading ", error_label, ": ", e$message))
    }
  )
}
