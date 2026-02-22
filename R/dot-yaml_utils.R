#' Clean YAML Structure
#'
#' Removes empty sections and cleans up YAML structure.
#'
#' @param data YAML data structure
#' @param remove_empty Logical; if TRUE, removes empty sections
#' @param preserve_sections Character vector of section names to always preserve
#' @return Cleaned data structure
#' @keywords internal
.clean_yaml_structure <- function(data, remove_empty = TRUE, preserve_sections = character(0)) {
  if (!is.list(data) || !remove_empty) {
    return(data)
  }

  sections_to_remove <- character(0)

  for (section_name in names(data)) {
    # Check if section should be preserved
    if (section_name %in% preserve_sections) {
      next
    }

    # Check if section is empty
    if (is.list(data[[section_name]]) && length(data[[section_name]]) == 0) {
      sections_to_remove <- c(sections_to_remove, section_name)
    }
  }

  # Remove empty sections
  for (section in sections_to_remove) {
    data[[section]] <- NULL
  }

  return(data)
}


#' Extract YAML Metadata Sections
#'
#' Separates data sections from metadata sections in a template structure.
#'
#' @param template_data Full template data structure
#' @return List with 'data' and 'metadata' components
#' @keywords internal
.separate_yaml_sections <- function(template_data) {
  metadata_sections <- .get_metadata_sections()

  data_sections <- list()
  metadata <- list()

  for (section_name in names(template_data)) {
    if (section_name %in% metadata_sections) {
      metadata[[section_name]] <- template_data[[section_name]]
    } else {
      data_sections[[section_name]] <- template_data[[section_name]]
    }
  }

  return(list(
    data = data_sections,
    metadata = metadata
  ))
}
