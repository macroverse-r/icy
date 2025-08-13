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
      .get_config_template(package = package, section = section, yaml_file = fn_tmpl)
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

#' Get Description for Variable from Template
#'
#' @param var_name Variable name
#' @param package Package name
#' @param fn_tmpl Optional path to custom YAML template file (NULL uses default template)
#' @return Character string with full description, or NULL if not found
#' @keywords internal
.get_description <- function(var_name, package, fn_tmpl = NULL) {
  .get_template_value(var_name, package, "descriptions", fn_tmpl = fn_tmpl)
}

#' Get Note for Variable from Template
#'
#' @param var_name Variable name
#' @param package Package name
#' @param fn_tmpl Optional path to custom YAML template file (NULL uses default template)
#' @return Character string with note, or NULL if not found
#' @keywords internal
.get_note <- function(var_name, package, fn_tmpl = NULL) {
  .get_template_value(var_name, package, "notes", fn_tmpl = fn_tmpl)
}

#' Get Type for Variable from Template
#'
#' Handles boolean → logical and dir → path normalization.
#'
#' @param var_name Variable name
#' @param package Package name
#' @param fn_tmpl Optional path to custom YAML template file (NULL uses default template)
#' @return Character string with normalized type, or NULL if not found
#' @keywords internal
.get_type <- function(var_name, package, fn_tmpl = NULL) {
  # Processor to normalize types
  normalize_type <- function(type) {
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
  
  .get_template_value(var_name, package, "types", normalize_type, fn_tmpl = fn_tmpl)
}

#' Get Option for Variable from Template
#'
#' @param var_name Variable name
#' @param package Package name
#' @param fn_tmpl Optional path to custom YAML template file (NULL uses default template)
#' @return Character vector with options, or NULL if not found
#' @keywords internal
.get_option <- function(var_name, package, fn_tmpl = NULL) {
  # Processor to convert to character
  to_character <- function(options) {
    return(as.character(options))
  }
  
  .get_template_value(var_name, package, "options", to_character, fn_tmpl = fn_tmpl)
}