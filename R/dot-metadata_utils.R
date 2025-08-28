#' Get Metadata Sections
#'
#' Returns the standard ordered list of metadata sections used in icy templates.
#' This centralizes the definition to ensure consistency across all functions.
#'
#' @return Character vector of metadata section names in standard order
#' @keywords internal
.get_metadata_sections <- function() {
  schema_file <- system.file("icy_metadata_sections.yml", package = "icy")
  
  if (file.exists(schema_file)) {
    tryCatch({
      schema <- yaml::read_yaml(schema_file)
      return(schema$metadata_sections)
    }, error = function(e) {
      # Fall through to fallback if YAML reading fails
    })
  }
  
  # Fallback to hardcoded values if YAML file not found or unreadable
  return(c("types", "descriptions", "notes", "options", "inheritances"))
}


#' Get Metadata Definitions
#'
#' Returns the definitions for metadata sections used when formatting 
#' template files with section comments.
#'
#' @return Named list of metadata section definitions, or NULL if unavailable
#' @keywords internal
.get_metadata_definitions <- function() {
  schema_file <- system.file("icy_metadata_sections.yml", package = "icy")
  
  # For development: if system.file returns empty, try inst/ directory
  if (nchar(schema_file) == 0 || !file.exists(schema_file)) {
    schema_file <- "inst/icy_metadata_sections.yml"
  }
  
  if (file.exists(schema_file)) {
    schema <- yaml::read_yaml(schema_file)
    return(schema$definitions)
  }
  
  # Return NULL if file not found
  return(NULL)
}


#' Get Header Template
#'
#' Returns the header template for generating template files.
#'
#' @param type Character string specifying header type ("template" or "local")
#' @return Character vector of header template lines, or NULL if unavailable
#' @keywords internal
.get_header_template <- function(type = "template") {
  schema_file <- system.file("icy_metadata_sections.yml", package = "icy")
  
  # For development: if system.file returns empty, try inst/ directory
  if (nchar(schema_file) == 0 || !file.exists(schema_file)) {
    schema_file <- "inst/icy_metadata_sections.yml"
  }
  
  if (file.exists(schema_file)) {
    schema <- yaml::read_yaml(schema_file)
    if (type %in% names(schema$header)) {
      return(schema$header[[type]])
    } else {
      return(schema$header$template)  # Fallback to template
    }
  }
  
  # Return NULL if file not found
  return(NULL)
}