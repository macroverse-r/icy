.icy_metadata_cache <- new.env(parent = emptyenv())


#' Read Metadata
#'
#' Reads and returns the icy metadata YAML file. This is the single
#' source of truth for metadata section names, definitions, and header templates.
#' Result is cached after first read since the data never changes at runtime.
#'
#' @return Parsed YAML list from icy_metadata_sections.yml
#' @keywords internal
.read_metadata <- function() {
  if (!is.null(.icy_metadata_cache$data)) return(.icy_metadata_cache$data)

  schema_file <- system.file("icy_metadata_sections.yml", package = "icy")

  if (!file.exists(schema_file)) {
    .icy_stop("icy metadata schema file not found. Package installation may be corrupted.")
  }

  .icy_metadata_cache$data <- yaml::read_yaml(schema_file)
  .icy_metadata_cache$data
}


#' Generate Header
#'
#' Unified function for generating headers for configuration files.
#' Supports template, config, and custom header types.
#'
#' @param package Character string with package name
#' @param type Character string specifying header type ("template", "config", "none",
#'   NULL, or custom character vector)
#' @param additional_lines Optional character vector of additional header lines
#' @param template_source Character string with path to template file for config files
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
  metadata <- .read_metadata()
  header_template <- metadata$header[[type]]

  if (is.null(header_template)) {
    .icy_stop(paste0("Unknown header type: '", type, "'. Valid types: ",
                     paste(c(names(metadata$header), "none"), collapse = ", "),
                     ", or a custom character vector."))
  }

  if (is.null(header_template$title) || is.null(header_template$description)) {
    .icy_stop(paste0("Header type '", type, "' is missing required 'title' or 'description' field in metadata schema."))
  }

  # Build header from title and description fields
  title <- gsub("\\{PACKAGE\\}", toupper(package), header_template$title)
  title <- gsub("\\{DATE\\}", as.character(Sys.Date()), title)

  description_lines <- sapply(header_template$description, function(line) {
    line <- gsub("\\{PACKAGE\\}", toupper(package), line)
    line <- gsub("\\{DATE\\}", as.character(Sys.Date()), line)
    if (!is.null(template_source)) {
      line <- gsub("\\{TEMPLATE_SOURCE\\}", basename(template_source), line)
    }
    return(line)
  }, USE.NAMES = FALSE)

  header <- c(
    paste("#", title),
    "#",
    paste("#", description_lines)
  )

  if (!is.null(additional_lines)) {
    header <- c(header, "#", additional_lines)
  }

  return(header)
}
