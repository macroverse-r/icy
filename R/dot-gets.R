#' Generic Template Config Getter
#'
#' Internal helper function to get configuration values from template.
#'
#' @param var_name Variable name
#' @param package Package name
#' @param section Config section (e.g., "descriptions", "types", "options", "notes")
#' @param processor Optional function to process the retrieved value
#' @param fn_tmpl Optional path to custom YAML template file (NULL uses default template)
#' @return Processed config value, or NULL if not found
#' @keywords internal
.get_template_value <- function(var_name, package, section, processor = NULL, fn_tmpl = NULL) {
  tryCatch({
    config <- if (is.null(fn_tmpl)) {
      get_config(package = package, origin = "template", section = section)
    } else {
      .get_config_template(package = package, section = section, resolved_template_path = fn_tmpl)
    }

    if (!is.null(config) && var_name %in% names(config)) {
      value <- config[[var_name]]

      # Apply processor function if provided
      if (!is.null(processor)) {
        value <- processor(value)
      }

      return(value)
    }

    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

#' Normalize Type Names
#'
#' Normalizes type names to standard R types.
#' Converts: boolean/bool -> logical, dir -> path
#'
#' @param type Character string with type name
#' @return Normalized type name
#' @keywords internal
.normalize_type <- function(type) {
  if (!is.null(type)) {
    if (type %in% c("boolean", "bool")) {
      return("logical")
    }
    if (type == "dir") {
      return("path")
    }
  }
  return(type)
}
