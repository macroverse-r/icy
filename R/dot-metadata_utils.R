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


#' Generate Header
#'
#' Unified function for generating headers for configuration files.
#' Supports template, local, and custom header types.
#'
#' @param package Character string with package name
#' @param type Character string specifying header type ("template", "local", "none", 
#'   NULL, or custom character vector)
#' @param additional_lines Optional character vector of additional header lines
#' @param template_source Character string with path to template file for local configs
#' @return Character vector of header lines, or character(0) for no header
#' @keywords internal
.generate_header <- function(package, type = "template", additional_lines = NULL, template_source = NULL) {
  # Handle special cases
  if (identical(type, "none") || is.null(type)) {
    return(character(0))
  }
  
  # Handle custom header provided as character vector
  if (is.character(type) && length(type) > 1) {
    header <- type
    if (!is.null(additional_lines)) {
      header <- c(header, "#", additional_lines)
    }
    return(header)
  }
  
  # Get header template from metadata
  schema_file <- system.file("icy_metadata_sections.yml", package = "icy")
  
  # For development: if system.file returns empty, try inst/ directory
  if (nchar(schema_file) == 0 || !file.exists(schema_file)) {
    schema_file <- "inst/icy_metadata_sections.yml"
  }
  
  if (!file.exists(schema_file)) {
    return(character(0))  # No header if metadata not found
  }
  
  schema <- yaml::read_yaml(schema_file)
  
  # Get appropriate header template
  header_template <- if (type %in% names(schema$header)) {
    schema$header[[type]]
  } else {
    schema$header$template  # Fallback to template
  }
  
  if (is.null(header_template)) {
    return(character(0))
  }
  
  # Handle new structure with title and description fields
  if ("title" %in% names(header_template) && "description" %in% names(header_template)) {
    # New structure: build header from title and description
    title <- gsub("\\{PACKAGE\\}", toupper(package), header_template$title)
    title <- gsub("\\{DATE\\}", as.character(Sys.Date()), title)
    
    description_lines <- sapply(header_template$description, function(line) {
      line <- gsub("\\{PACKAGE\\}", toupper(package), line)
      line <- gsub("\\{DATE\\}", as.character(Sys.Date()), line)
      # Handle template source for local configs
      if (!is.null(template_source)) {
        line <- gsub("\\{TEMPLATE_SOURCE\\}", basename(template_source), line)
      }
      return(line)
    }, USE.NAMES = FALSE)
    
    # Build header: title, blank line, description lines (all with # prefix)
    header <- c(
      paste("#", title),
      "#",
      paste("#", description_lines)
    )
  }
  
  # Add additional lines if provided
  if (!is.null(additional_lines)) {
    header <- c(header, "#", additional_lines)
  }
  
  return(header)
}
