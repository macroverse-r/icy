#' Validate Configuration File
#'
#' Validates a configuration file (template or local) according to its type.
#' Both template and local files must have an inheritances section, but
#' different validation rules apply to each type.
#'
#' @param file_path Path to the configuration file to validate
#' @param type Type of configuration file: "template" or "local"
#' @param package Package name for context. If NULL, attempts to detect.
#' @param template_path Path to template file (required for local validation)
#' @param verbose Logical. If TRUE, shows detailed validation messages
#' @param quick Logical. If TRUE, performs minimal validation for performance
#'
#' @return List with validation results containing:
#'   \describe{
#'     \item{valid}{Logical indicating if validation passed}
#'     \item{errors}{Character vector of error messages}
#'     \item{warnings}{Character vector of warning messages}
#'   }
#'
#' @details
#' Template validation checks:
#' \itemize{
#'   \item Must have inheritances section (can be empty)
#'   \item Validates inheritance relationships
#'   \item Checks metadata sections (types, descriptions, options, notes)
#'   \item Validates section structure
#' }
#'
#' Local validation checks:
#' \itemize{
#'   \item Must have inheritances section (can be empty)
#'   \item Validates values against template's types
#'   \item Checks variable names exist in template
#'   \item Validates values are within template's options if defined
#'   \item Validates inheritance relationships
#' }
#'
#' @examples
#' \dontrun{
#' # Validate a template file
#' result <- validate_config_file("inst/mypackage_config_template.yml", type = "template")
#' 
#' # Validate a local config file
#' result <- validate_config_file("~/.config/R/mypackage/config.yml", 
#'                                type = "local",
#'                                template_path = "inst/mypackage_config_template.yml")
#' }
#'
#' @export
validate_config_file <- function(file_path,
                                type = c("template", "local"),
                                package = NULL,
                                template_path = NULL,
                                verbose = FALSE,
                                quick = FALSE) {
  
  type <- match.arg(type)
  
  # Initialize result
  result <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    file = file_path,
    type = type
  )
  
  # Check file exists
  if (!file.exists(file_path)) {
    result$valid <- FALSE
    result$errors <- c(result$errors, paste0("File not found: ", file_path))
    return(result)
  }
  
  # Read the configuration file
  config_data <- tryCatch({
    yaml::read_yaml(file_path)
  }, error = function(e) {
    result$valid <- FALSE
    result$errors <- c(result$errors, paste0("Failed to parse YAML: ", e$message))
    return(NULL)
  })
  
  if (is.null(config_data)) {
    return(result)
  }
  
  # Both types MUST have inheritances section
  if (!"inheritances" %in% names(config_data)) {
    result$valid <- FALSE
    result$errors <- c(result$errors, 
                      sprintf("%s config must include an 'inheritances' section (can be empty)",
                              tools::toTitleCase(type)))
    return(result)
  }
  
  # Validate inheritances structure (common to both types)
  inheritance_result <- .validate_config_inheritance(config_data)
  if (!inheritance_result$valid) {
    result$valid <- FALSE
    result$errors <- c(result$errors, inheritance_result$errors)
  }
  result$warnings <- c(result$warnings, inheritance_result$warnings)
  
  # Type-specific validation
  if (type == "template") {
    # Template-specific validation
    template_result <- .validate_template_specific(config_data, package, verbose)
    result$valid <- result$valid && template_result$valid
    result$errors <- c(result$errors, template_result$errors)
    result$warnings <- c(result$warnings, template_result$warnings)
    
  } else if (type == "local") {
    # Local-specific validation requires template for comparison
    if (is.null(template_path)) {
      # Try to find template automatically
      if (is.null(package)) {
        package <- get_package_name(verbose = FALSE)
      }
      
      files <- find_config_files(package = package, verbose = FALSE)
      template_path <- files$fn_tmpl
      
      if (is.null(template_path)) {
        result$warnings <- c(result$warnings, 
                           "Cannot validate against template: template file not found")
        return(result)
      }
    }
    
    # Read template for validation
    template_data <- tryCatch({
      yaml::read_yaml(template_path)
    }, error = function(e) {
      result$warnings <- c(result$warnings,
                         paste0("Cannot read template for validation: ", e$message))
      return(NULL)
    })
    
    if (!is.null(template_data)) {
      local_result <- .validate_local_specific(config_data, template_data, verbose)
      result$valid <- result$valid && local_result$valid
      result$errors <- c(result$errors, local_result$errors)
      result$warnings <- c(result$warnings, local_result$warnings)
    }
  }
  
  return(result)
}


#' Validate Configuration Inheritance
#' 
#' Internal function to validate inheritance relationships in a config file.
#' Used by both template and local validation.
#' 
#' @keywords internal
.validate_config_inheritance <- function(config_data) {
  result <- list(
    valid = TRUE,
    errors = character(),
    warnings = character()
  )
  
  inherit_map <- config_data$inheritances
  
  # Allow NULL/empty inheritances
  if (is.null(inherit_map) || length(inherit_map) == 0) {
    return(result)
  }
  
  # Must be a list if not empty
  if (!is.list(inherit_map)) {
    result$valid <- FALSE
    result$errors <- c(result$errors, "Inheritances must be a named list")
    return(result)
  }
  
  # Get data sections (non-metadata)
  metadata_sections <- .get_metadata_sections()
  data_sections <- setdiff(names(config_data), metadata_sections)
  
  # Check inheritance relationships
  for (section in names(inherit_map)) {
    parent <- inherit_map[[section]]
    
    if (is.null(parent)) next
    
    # Check section exists
    if (!section %in% data_sections) {
      result$warnings <- c(result$warnings,
                          paste0("Inheritance defined for non-existent section: '", section, "'"))
    }
    
    # Check parent exists
    if (!parent %in% data_sections) {
      result$valid <- FALSE
      result$errors <- c(result$errors,
                        paste0("Section '", section, "' inherits from non-existent section: '", parent, "'"))
    }
    
    # Check for self-inheritance
    if (section == parent) {
      result$valid <- FALSE
      result$errors <- c(result$errors,
                        paste0("Section '", section, "' cannot inherit from itself"))
    }
  }
  
  # Check for circular dependencies
  for (section in names(inherit_map)) {
    visited <- character()
    current <- section
    
    while (!is.null(current) && current %in% names(inherit_map)) {
      if (current %in% visited) {
        cycle_path <- c(visited[seq(which(visited == current), length(visited))], current)
        result$valid <- FALSE
        result$errors <- c(result$errors,
                          paste0("Circular inheritance detected: ", 
                                paste(cycle_path, collapse = " -> ")))
        break
      }
      visited <- c(visited, current)
      current <- inherit_map[[current]]
    }
  }
  
  return(result)
}


#' Validate Template-Specific Requirements
#' 
#' @keywords internal
.validate_template_specific <- function(config_data, package, verbose) {
  result <- list(
    valid = TRUE,
    errors = character(),
    warnings = character()
  )
  
  # Check for required metadata sections
  metadata_sections <- c("types", "descriptions")
  
  for (section in metadata_sections) {
    if (!section %in% names(config_data)) {
      result$warnings <- c(result$warnings,
                          paste0("Template is missing recommended section: '", section, "'"))
    }
  }
  
  # Validate types section if present
  if ("types" %in% names(config_data)) {
    types <- config_data$types
    if (!is.null(types) && !is.list(types)) {
      result$valid <- FALSE
      result$errors <- c(result$errors, "Types section must be a named list")
    } else if (!is.null(types)) {
      valid_types <- c("character", "logical", "integer", "numeric", "path")
      for (var in names(types)) {
        if (!types[[var]] %in% valid_types) {
          result$warnings <- c(result$warnings,
                              paste0("Unknown type '", types[[var]], "' for variable '", var, "'"))
        }
      }
    }
  }
  
  # Validate options section if present
  if ("options" %in% names(config_data)) {
    options <- config_data$options
    if (!is.null(options) && !is.list(options)) {
      result$valid <- FALSE
      result$errors <- c(result$errors, "Options section must be a named list")
    }
  }
  
  return(result)
}


#' Validate Local-Specific Requirements
#' 
#' @keywords internal
.validate_local_specific <- function(config_data, template_data, verbose) {
  result <- list(
    valid = TRUE,
    errors = character(),
    warnings = character()
  )
  
  # Get types from template
  template_types <- if ("types" %in% names(template_data)) {
    template_data$types
  } else {
    list()
  }
  
  # Get options from template
  template_options <- if ("options" %in% names(template_data)) {
    template_data$options
  } else {
    list()
  }
  
  # Get all template variables (from default section)
  template_vars <- if ("default" %in% names(template_data)) {
    names(template_data$default)
  } else {
    character()
  }
  
  # Check each section in local config
  metadata_sections <- .get_metadata_sections()
  data_sections <- setdiff(names(config_data), c(metadata_sections, "inheritances"))
  
  for (section in data_sections) {
    if (!is.list(config_data[[section]])) next
    
    for (var_name in names(config_data[[section]])) {
      value <- config_data[[section]][[var_name]]
      
      # Check if variable exists in template
      if (length(template_vars) > 0 && !var_name %in% template_vars) {
        result$warnings <- c(result$warnings,
                           paste0("Variable '", var_name, "' in section '", section,
                                 "' not defined in template"))
      }
      
      # Validate type if specified in template
      if (var_name %in% names(template_types)) {
        expected_type <- template_types[[var_name]]
        type_result <- .validate_variable_type(value, expected_type, var_name)
        if (!type_result$valid) {
          result$warnings <- c(result$warnings, type_result$message)
        }
      }
      
      # Validate against options if specified
      if (var_name %in% names(template_options)) {
        valid_options <- template_options[[var_name]]
        if (!is.null(valid_options) && length(valid_options) > 0) {
          if (!value %in% valid_options) {
            result$warnings <- c(result$warnings,
                               paste0("Value '", value, "' for '", var_name,
                                     "' not in allowed options: ",
                                     paste(valid_options, collapse = ", ")))
          }
        }
      }
    }
  }
  
  return(result)
}