#' Process a Config Section (Internal)
#'
#' Extracts a section from parsed YAML data, resolves variable references
#' (\code{${VAR_NAME}} patterns), and resolves path-type variables using
#' template types. Pure data transformation, no file I/O.
#'
#' @param config_data Parsed YAML data (full file, all sections).
#' @param section Character string for the section to extract (default: "default").
#' @param template_types Named list of variable types from the template.
#'   Used to identify path-type variables for resolution. Can be NULL.
#' @param package Character string with the package name. Used for path resolution.
#' @param source_label Label for error messages (e.g., "config", "template").
#'
#' @return Named list of configuration values from the requested section.
#' @keywords internal
.process_config_section <- function(config_data,
                                    section = "default",
                                    template_types = NULL,
                                    package = NULL,
                                    source_label = "config") {
  if (is.null(config_data)) {
    return(list())
  }

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
