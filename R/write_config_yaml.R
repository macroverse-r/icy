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
#'   \item Local configuration files (via write_local2)
#'   \item Template files (via create_template/update_template)
#'   \item Any package-related YAML configuration
#' }
#'
#' It integrates with icy's validation system and properly handles NULL values
#' by preserving them as `~` in the YAML output.
#'
#' @examples
#' \dontrun{
#' # Write to a local configuration
#' write_config_yaml(
#'   var_list = list(API_KEY = "secret", DEBUG = TRUE),
#'   file_path = find_local("mypackage"),
#'   package = "mypackage"
#' )
#'
#' # Write a template with header
#' write_config_yaml(
#'   var_list = list(VAR1 = NULL, VAR2 = "default"),
#'   file_path = "inst/mypackage_config_template.yml",
#'   package = "mypackage",
#'   custom_header = c("# MyPackage Configuration Template", "")
#' )
#' }
#'
#' @export
write_config_yaml <- function(var_list,
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
  existing_header <- character(0)
  config_data <- if (file.exists(file_path)) {
    # Extract header if no custom header provided
    if (is.null(custom_header)) {
      existing_header <- .extract_yaml_header(file_path)
    }
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
  
  # Determine header to use
  header_lines <- if (!is.null(custom_header)) {
    custom_header
  } else if (length(existing_header) > 0) {
    existing_header
  } else {
    character(0)
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
  # Convert to YAML
  yaml_content <- yaml::as.yaml(config_data)
  yaml_lines <- strsplit(yaml_content, "\n")[[1]]
  
  # Ensure header ends with blank line if it has content
  if (length(header_lines) > 0 && !identical(header_lines[length(header_lines)], "")) {
    header_lines <- c(header_lines, "")
  }
  
  # Combine header and content
  complete_content <- c(header_lines, yaml_lines)
  
  # Write to file
  writeLines(complete_content, file_path)
}


#' Extract YAML Header Comments
#' @keywords internal
.extract_yaml_header <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  header_lines <- character(0)
  
  for (line in lines) {
    # Include comment lines and blank lines at the start
    if (grepl("^\\s*#", line) || nchar(trimws(line)) == 0) {
      header_lines <- c(header_lines, line)
    } else {
      # Stop at first non-comment, non-blank line
      break
    }
  }
  
  # Remove trailing blank lines from header
  while (length(header_lines) > 0 && 
         nchar(trimws(header_lines[length(header_lines)])) == 0) {
    header_lines <- header_lines[-length(header_lines)]
  }
  
  return(header_lines)
}