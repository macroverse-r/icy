



#' Prepare Data for YAML Writing
#'
#' Prepares data structure for YAML serialization, handling special cases
#' and ensuring proper structure.
#'
#' @param data List or data structure to prepare
#' @return Prepared YAML string
#' @keywords internal
.prepare_yaml_for_writing <- function(data) {
  # Convert to YAML
  yaml_str <- yaml::as.yaml(data)
  
  return(yaml_str)
}


#' Merge YAML Data Structures
#'
#' Intelligently merges two YAML data structures, with options for
#' how to handle conflicts.
#'
#' @param base_data Base data structure (will be modified)
#' @param new_data New data to merge in
#' @param overwrite Logical; if TRUE, new values overwrite existing
#' @param deep_merge Logical; if TRUE, performs deep merge of nested structures
#' @return Merged data structure
#' @keywords internal
.merge_yaml_data <- function(base_data, new_data, overwrite = TRUE, deep_merge = TRUE) {
  if (is.null(base_data)) {
    return(new_data)
  }
  
  if (is.null(new_data)) {
    return(base_data)
  }
  
  # If not lists, just return based on overwrite setting
  if (!is.list(base_data) || !is.list(new_data)) {
    return(if (overwrite) new_data else base_data)
  }
  
  # Merge lists
  for (key in names(new_data)) {
    if (key %in% names(base_data)) {
      if (deep_merge && is.list(base_data[[key]]) && is.list(new_data[[key]])) {
        # Recursive merge for nested lists
        base_data[[key]] <- .merge_yaml_data(
          base_data[[key]], 
          new_data[[key]], 
          overwrite = overwrite,
          deep_merge = TRUE
        )
      } else if (overwrite) {
        base_data[[key]] <- new_data[[key]]
      }
      # If not overwriting and not deep merging, keep existing value
    } else {
      # New key, always add
      base_data[[key]] <- new_data[[key]]
    }
  }
  
  return(base_data)
}


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
  metadata_sections <- c("descriptions", "types", "notes", "options")
  
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


#' Reconstruct Template Structure
#'
#' Reconstructs a complete template structure from separated data and metadata.
#'
#' @param data_sections Data sections (default, production, etc.)
#' @param metadata Metadata sections (descriptions, types, etc.)
#' @return Complete template structure
#' @keywords internal
.reconstruct_template <- function(data_sections, metadata) {
  # Start with data sections
  template_data <- data_sections
  
  # Add metadata sections in standard order
  metadata_order <- c("descriptions", "types", "notes", "options")
  
  for (section in metadata_order) {
    if (section %in% names(metadata) && length(metadata[[section]]) > 0) {
      template_data[[section]] <- metadata[[section]]
    }
  }
  
  return(template_data)
}


