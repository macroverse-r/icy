#' Write Configuration to YAML File
#'
#' Package-aware YAML configuration writer for the icy ecosystem that can write
#' to both local configuration files and template files with support for custom
#' headers, NULL handling, and validation.
#'
#' @param var_list Named list of variables to write. Names should be the
#'   variable names and values should be the values to set.
#' @param file_path Character string with the target file path (absolute or relative).
#'   For local configs, use find_local() to get this path.
#' @param package Character string with the package name. Defaults to `get_package_name()`
#'   to detect the calling package. Used for validation against templates.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param template_file Character string with path to template file for validation.
#'   If NULL and package is provided, will look for the package's template.
#' @param create_if_missing Logical; if TRUE (default), creates parent directories
#'   if they don't exist. The file itself is always created/updated.
#' @param custom_header Character vector of header lines to add to the file.
#'   Each element becomes a line. If NULL, preserves existing header or adds none.
#' @param append_sections Logical; if FALSE (default for templates), replaces the
#'   entire section. If TRUE (default for configs), merges with existing variables.
#' @param strict_template Logical; if TRUE, removes any variables not defined in the
#'   template (used for local configs). If FALSE (default), keeps all variables.
#' @param verbose Logical. If TRUE, displays informative messages. Defaults to FALSE.
#'
#' @return Invisibly returns the file path on success.
#'
#' @details
#' This function is the core YAML writer for the icy ecosystem, designed to handle:
#' \itemize{
#'   \item Local configuration files (via write_local)
#'   \item Template files (via create_template/update_template)
#'   \item Any package-related YAML configuration
#' }
#'
#' It integrates with icy's validation system and properly handles NULL values
#' by preserving them as `~` in the YAML output.
#'
#' @keywords internal
.write_config_yaml <- function(var_list,
                              file_path,
                              package = get_package_name(verbose = FALSE),
                              section = "default",
                              template_file = NULL,
                              create_if_missing = TRUE,
                              custom_header = NULL,
                              append_sections = TRUE,
                              strict_template = FALSE,
                              verbose = FALSE) {
  
  # Basic input validation
  if (!is.list(var_list)) {
    .icy_stop("var_list must be a list")
  }
  if (missing(file_path) || is.null(file_path)) {
    .icy_stop("file_path is required")
  }
  
  # Validate against template if package context exists
  valid_vars <- NULL  # Initialize for later use in ordering
  if (!is.null(package) && length(var_list) > 0) {
    # Use icy's validation system
    if (!is.null(template_file)) {
      # Specific template file provided
      valid_vars <- tryCatch({
        template_config <- yaml::read_yaml(template_file)
        if (section != "" && section %in% names(template_config)) {
          names(template_config[[section]])
        } else {
          names(template_config)
        }
      }, error = function(e) {
        .icy_stop(paste0("Could not read template file: ", e$message))
      })
    } else {
      # Use package's default template via get_config
      valid_vars <- tryCatch({
        template_config <- get_config(package = package, origin = "template", section = section)
        names(template_config)
      }, error = function(e) {
        # No template found is OK for some operations
        NULL
      })
    }
    
    # Validate if we have valid vars
    if (!is.null(valid_vars)) {
      invalid_vars <- setdiff(names(var_list), valid_vars)
      if (length(invalid_vars) > 0) {
        .icy_stop(paste0(
          "Invalid variable", if(length(invalid_vars) > 1) "s" else "", ": ",
          paste(invalid_vars, collapse = ", "), "\n",
          "Valid variables: ", paste(valid_vars, collapse = ", ")
        ))
      }
    }
  }
  
  # Ensure directory exists
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path) && create_if_missing) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Read existing file if present
  file_exists <- file.exists(file_path)
  config_data <- if (file_exists) {
    # Read existing data
    tryCatch(
      yaml::read_yaml(file_path),
      error = function(e) {
        .icy_warn(paste0("Could not parse existing YAML: ", e$message))
        list()
      }
    )
  } else {
    list()
  }
  
  # Update the configuration data
  if (!append_sections || length(config_data) == 0) {
    # Replace mode or new file
    if (section != "" && !is.null(section)) {
      config_data <- list()
      config_data[[section]] <- var_list
    } else {
      config_data <- var_list
    }
  } else {
    # Append/merge mode
    if (section != "" && !is.null(section)) {
      config_data <- .update_yaml_section(config_data, var_list, section)
    } else {
      config_data <- .update_yaml_root(config_data, var_list)
    }
  }
  
  # Reorder variables to match template order if we have valid_vars
  if (!is.null(valid_vars) && section != "" && !is.null(section)) {
    if (section %in% names(config_data)) {
      current_vars <- names(config_data[[section]])
      if (strict_template) {
        # For local configs: only keep variables that are in the template
        # This matches write_local behavior (line 157)
        ordered_vars <- intersect(valid_vars, current_vars)
      } else {
        # For templates and other uses: keep all vars but order template vars first
        ordered_vars <- c(
          intersect(valid_vars, current_vars),  # Template vars in template order
          setdiff(current_vars, valid_vars)      # Non-template vars at end
        )
      }
      config_data[[section]] <- config_data[[section]][ordered_vars]
    }
  }
  
  # Determine header to use - always generate fresh header
  header_lines <- if (!is.null(custom_header)) {
    custom_header
  } else {
    # Generate fresh header with appropriate date label
    .generate_template_header(package, is_update = file_exists)
  }
  
  # Write to file
  if (length(header_lines) > 0) {
    # Use custom writer for headers
    .write_yaml_with_header(config_data, file_path, header_lines)
  } else {
    # Use standard yaml::write_yaml
    yaml::write_yaml(config_data, file_path)
  }
  
  if (verbose) {
    .icy_success(paste0("Successfully wrote to: ", file_path))
  }
  
  invisible(file_path)
}


#' Update YAML Section with NULL Preservation
#' @keywords internal
.update_yaml_section <- function(config_data, var_list, section) {
  if (!section %in% names(config_data)) {
    config_data[[section]] <- list()
  }
  
  # Track which variables are NULL
  null_vars <- names(var_list)[sapply(var_list, is.null)]
  
  # Update section with new values
  for (var_name in names(var_list)) {
    config_data[[section]][[var_name]] <- var_list[[var_name]]
  }
  
  # Add back NULL values using method that preserves them
  for (null_var in null_vars) {
    temp_list <- list(NULL)
    names(temp_list) <- null_var
    config_data[[section]] <- c(config_data[[section]], temp_list)
  }
  
  return(config_data)
}


#' Update YAML Root Level with NULL Preservation
#' @keywords internal
.update_yaml_root <- function(config_data, var_list) {
  # Track which variables are NULL
  null_vars <- names(var_list)[sapply(var_list, is.null)]
  
  # Update with new values
  for (var_name in names(var_list)) {
    config_data[[var_name]] <- var_list[[var_name]]
  }
  
  # Add back NULL values
  for (null_var in null_vars) {
    temp_list <- list(NULL)
    names(temp_list) <- null_var
    config_data <- c(config_data, temp_list)
  }
  
  return(config_data)
}


#' Write YAML with Header Comments
#' @keywords internal
.write_yaml_with_header <- function(config_data, file_path, header_lines) {
  # Check if this is a template structure
  if (.is_template_structure(config_data)) {
    # Use formatted template writer with section comments
    yaml_lines <- .format_yaml_with_section_comments(config_data)
  } else {
    # Use standard YAML formatting
    yaml_content <- yaml::as.yaml(config_data)
    yaml_lines <- strsplit(yaml_content, "\n")[[1]]
  }
  
  # Combine header and content with one blank line between
  complete_content <- c(header_lines, "", yaml_lines)
  
  # Write to file
  writeLines(complete_content, file_path)
}


#' Extract YAML Header Comments
#' @keywords internal
.extract_yaml_header <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  header_lines <- character(0)
  
  for (line in lines) {
    # Only include comment lines - stop at first blank line or non-comment
    if (grepl("^\\s*#", line)) {
      header_lines <- c(header_lines, line)
    } else {
      # Stop at first non-comment line (including blank lines)
      break
    }
  }
  
  return(header_lines)
}


#' Format YAML with Descriptive Section Comments
#'
#' Manually formats template YAML with comprehensive section descriptions
#' and proper spacing for better readability and documentation.
#'
#' @param config_data Template data structure
#' @return Character vector of formatted YAML lines
#' @keywords internal
.format_yaml_with_section_comments <- function(config_data) {
  yaml_lines <- character(0)
  
  # Define the standard order: data sections first, then metadata in specific order
  metadata_sections <- .get_metadata_sections()
  
  # Get section descriptions early for use in both data and metadata sections
  section_descriptions <- .get_metadata_definitions()
  
  # Separate data sections from metadata sections
  all_sections <- names(config_data)
  data_sections <- setdiff(all_sections, metadata_sections)
  metadata_present <- intersect(metadata_sections, all_sections)
  
  # Ensure 'default' comes first in data sections
  data_sections <- c(
    intersect("default", data_sections),
    setdiff(data_sections, "default")
  )
  
  # Helper function to format a section - self-contained with trailing space
  format_section <- function(section_name, section_data, comment_lines) {
    lines <- c(comment_lines, paste0(section_name, ":"))
    
    if (!is.null(section_data) && length(section_data) > 0) {
      # Non-empty section - convert to YAML and indent
      section_yaml <- yaml::as.yaml(list(temp = section_data))
      section_lines <- strsplit(section_yaml, "\n")[[1]]
      # Remove the "temp:" wrapper and add proper indentation
      section_lines <- section_lines[-1]  # Remove first line (temp:)
      if (length(section_lines) > 0) {
        lines <- c(lines, section_lines)
      }
    }
    
    # Add trailing blank line for ALL sections (empty or not)
    lines <- c(lines, "")
    
    return(lines)
  }
  
  # Collect all sections first
  all_sections_ordered <- c(data_sections, metadata_present)
  formatted_sections <- list()
  
  # Process data sections
  for (section in data_sections) {
    # Try to get section definition from centralized YAML
    section_def <- NULL
    if (!is.null(section_descriptions) && section %in% names(section_descriptions)) {
      section_def <- section_descriptions[[section]]
    }
    
    if (!is.null(section_def)) {
      # Use centralized definition
      comment_lines <- c(
        paste("#", section_def$title),
        section_def$description
      )
    } else {
      # Fallback for other data sections
      comment_lines <- c(
        paste("#", section, "environment configuration"),
        paste("# This section contains values specific to", section, "environments. When active,"),
        "# these values override the defaults. Use inheritance to avoid duplicating values."
      )
    }
    
    formatted_sections[[section]] <- format_section(section, config_data[[section]], comment_lines)
  }
  
  # Process metadata sections
  for (section in metadata_present) {
    info <- section_descriptions[[section]]
    comment_lines <- c(
      paste("#", info$title),
      info$description
    )
    
    formatted_sections[[section]] <- format_section(section, config_data[[section]], comment_lines)
  }
  
  # Assemble sections - each section handles its own spacing
  for (i in seq_along(all_sections_ordered)) {
    section <- all_sections_ordered[i]
    if (i == 1) {
      # First section
      yaml_lines <- formatted_sections[[section]]
    } else {
      # Subsequent sections - just concatenate (sections handle their own spacing)
      yaml_lines <- c(yaml_lines, formatted_sections[[section]])
    }
  }
  
  # Remove any trailing empty lines
  while (length(yaml_lines) > 0 && nchar(trimws(yaml_lines[length(yaml_lines)])) == 0) {
    yaml_lines <- yaml_lines[-length(yaml_lines)]
  }
  
  return(yaml_lines)
}


#' Check if Data Structure is a Template
#'
#' Determines if the config data represents a template file structure
#' by checking for the presence of metadata sections.
#'
#' @param config_data Configuration data structure
#' @return Logical indicating if this is a template structure
#' @keywords internal
.is_template_structure <- function(config_data) {
  metadata_sections <- .get_metadata_sections()
  metadata_present <- intersect(names(config_data), metadata_sections)
  
  # Consider it a template if it has at least 2 metadata sections or
  # has typical template sections
  return(length(metadata_present) >= 2 || "descriptions" %in% names(config_data))
}