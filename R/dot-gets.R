#' Generic Template Config Getter
#'
#' Internal helper function to get configuration values from template.
#'
#' @param var_name Variable name
#' @param package Package name
#' @param section Config section (e.g., "descriptions", "types", "options", "notes")
#' @param processor Optional function to process the retrieved value
#' @return Processed config value, or NULL if not found
#' @keywords internal
.get_template_value <- function(var_name, package, section, processor = NULL) {
  tryCatch({
    config <- get_config(package = package, origin = "template", user = section)
    
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
#' @return Character string with full description, or NULL if not found
#' @keywords internal
.get_description <- function(var_name, package) {
  .get_template_value(var_name, package, "descriptions")
}

#' Get Note for Variable from Template
#'
#' @param var_name Variable name
#' @param package Package name
#' @return Character string with note, or NULL if not found
#' @keywords internal
.get_note <- function(var_name, package) {
  .get_template_value(var_name, package, "notes")
}

#' Get Type for Variable from Template
#'
#' Handles boolean → logical normalization.
#'
#' @param var_name Variable name
#' @param package Package name
#' @return Character string with normalized type, or NULL if not found
#' @keywords internal
.get_type <- function(var_name, package) {
  # Processor to normalize boolean types
  normalize_type <- function(type) {
    if (!is.null(type) && type %in% c("boolean", "bool")) {
      return("logical")
    }
    return(type)
  }
  
  .get_template_value(var_name, package, "types", normalize_type)
}

#' Get Option for Variable from Template
#'
#' @param var_name Variable name
#' @param package Package name
#' @return Character vector with options, or NULL if not found
#' @keywords internal
.get_option <- function(var_name, package) {
  # Processor to convert to character
  to_character <- function(options) {
    return(as.character(options))
  }
  
  .get_template_value(var_name, package, "options", to_character)
}