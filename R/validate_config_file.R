#' Validate Configuration File
#'
#' Validates a configuration file (template or local) according to its type.
#' Both template and local files must have an inheritances section, but
#' different validation rules apply to each type.
#'
#' @param fn_tmpl Path to template configuration file. If NULL, will attempt to
#'   find using package name. Used for validation when type="template" or as
#'   reference when type="local".
#' @param fn_local Path to local configuration file. If NULL, will attempt to
#'   find using package name. Used for validation when type="local".
#' @param type Type of configuration file to validate: "template" or "local"
#' @param package Package name for context. If NULL, attempts to detect.
#' @param verbose Logical. If TRUE, shows detailed validation messages
#'
#' @return List with validation results containing:
#'   \describe{
#'     \item{valid}{Logical indicating if validation passed}
#'     \item{errors}{Character vector of error messages}
#'     \item{warnings}{Character vector of warning messages}
#'     \item{info}{Character vector of informational messages (templates only)}
#'     \item{structure}{Template structure information (templates only)}
#'     \item{inheritance}{Inheritance validation results (templates only)}
#'     \item{consistency}{Variable consistency results (templates only)}
#'   }
#'
#' @details
#' Template validation checks:
#' \itemize{
#'   \item Must have inheritances section (can be empty)
#'   \item Validates structure including section and variable naming
#'   \item Validates inheritance relationships and circular dependencies
#'   \item Checks consistency between data and metadata sections
#'   \item Validates types match declared values
#'   \item Checks for orphaned metadata
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
#' result <- validate_config_file(type = "template")
#' 
#' # Validate a local config file
#' result <- validate_config_file(type = "local")
#' 
#' # Validate specific files
#' result <- validate_config_file(fn_tmpl = "inst/mypackage_config_template.yml",
#'                                type = "template")
#' 
#' # Validate local with specific template for comparison
#' result <- validate_config_file(fn_local = "~/.config/R/mypackage/config.yml",
#'                                fn_tmpl = "inst/mypackage_config_template.yml", 
#'                                type = "local")
#' }
#'
#' @export
validate_config_file <- function(fn_tmpl = NULL,
                                fn_local = NULL,
                                type = c("template", "local"),
                                package = NULL,
                                verbose = FALSE) {
  
  type <- match.arg(type)
  
  # Find config files if not provided
  if (is.null(fn_tmpl) || is.null(fn_local)) {
    if (is.null(package)) {
      package <- get_package_name(verbose = FALSE)
    }
    
    files <- find_config_files(package = package, verbose = FALSE)
    
    if (is.null(fn_tmpl)) {
      fn_tmpl <- files$fn_tmpl
    }
    
    if (is.null(fn_local)) {
      fn_local <- files$fn_local
    }
  }
  
  # Determine which file to validate based on type
  file_to_validate <- if (type == "template") fn_tmpl else fn_local
  
  # Initialize result structure based on type
  if (type == "template") {
    result <- list(
      valid = TRUE,
      errors = character(),
      warnings = character(),
      info = character(),
      inheritance = list(valid = TRUE),
      structure = list(),
      consistency = list(),
      file = file_to_validate,
      type = type
    )
  } else {
    result <- list(
      valid = TRUE,
      errors = character(),
      warnings = character(),
      file = file_to_validate,
      type = type
    )
  }
  
  # Check if the file to validate exists
  if (is.null(file_to_validate) || !file.exists(file_to_validate)) {
    result$valid <- FALSE
    if (is.null(file_to_validate)) {
      result$errors <- c(result$errors, 
                        paste0("No ", type, " configuration file found for package '", package, "'"))
    } else {
      result$errors <- c(result$errors, paste0("File not found: ", file_to_validate))
    }
    return(result)
  }
  
  if (type == "template") {
    result$info <- c(result$info, paste0("Validating template: ", file_to_validate))
  }
  
  # Read the configuration file
  config_data <- tryCatch({
    yaml::read_yaml(file_to_validate)
  }, error = function(e) {
    result$valid <- FALSE
    result$errors <- c(result$errors, paste0("Invalid YAML syntax: ", e$message))
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
  
  # Type-specific validation
  if (type == "template") {
    # Template uses comprehensive validation
    
    # Structure validation
    structure_result <- .validate_template_structure(config_data)
    result$structure <- structure_result
    if (!structure_result$valid) {
      result$valid <- FALSE
      result$errors <- c(result$errors, structure_result$errors)
    }
    result$warnings <- c(result$warnings, structure_result$warnings)
    
    # Inheritance validation
    if ("inheritances" %in% names(config_data)) {
      inheritance_result <- .validate_template_inheritance(config_data)
      result$inheritance <- inheritance_result
      if (!inheritance_result$valid) {
        result$valid <- FALSE
        result$errors <- c(result$errors, inheritance_result$errors)
      }
      result$warnings <- c(result$warnings, inheritance_result$warnings)
    }
    
    # Variable consistency validation
    consistency_result <- .validate_template_variables(config_data)
    result$consistency <- consistency_result
    if (!consistency_result$valid) {
      result$valid <- FALSE
      result$errors <- c(result$errors, consistency_result$errors)
    }
    result$warnings <- c(result$warnings, consistency_result$warnings)
    
    # Type validation if types are present
    if ("types" %in% names(config_data)) {
      types_result <- .validate_template_types(config_data, package)
      if (!types_result$valid) {
        result$valid <- FALSE
        result$errors <- c(result$errors, types_result$errors)
      }
      result$warnings <- c(result$warnings, types_result$warnings)
    }
    
  } else if (type == "local") {
    # Local validation - first validate inheritance, then specific checks
    
    # Validate inheritances structure (simpler version for local)
    inheritance_result <- .validate_config_inheritance(config_data)
    if (!inheritance_result$valid) {
      result$valid <- FALSE
      result$errors <- c(result$errors, inheritance_result$errors)
    }
    result$warnings <- c(result$warnings, inheritance_result$warnings)
    
    # Local-specific validation requires template for comparison
    if (is.null(fn_tmpl)) {
      result$warnings <- c(result$warnings, 
                         "Cannot validate against template: template file not found")
      return(result)
    }
    
    # Read template for validation
    template_data <- tryCatch({
      yaml::read_yaml(fn_tmpl)
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


#' Validate Configuration Inheritance (Simple)
#' 
#' Simple inheritance validation for local configs.
#' Checks basic structure and circular dependencies.
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


#' Validate Template Inheritance
#'
#' Comprehensive inheritance validation for templates.
#' Checks for circular dependencies and validates inheritance chains.
#'
#' @param template_data The parsed YAML template data
#' @param max_depth Maximum allowed inheritance depth (default: 5)
#' @return List with validation results
#' @keywords internal
.validate_template_inheritance <- function(template_data, max_depth = 5) {
  result <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    graph = list(),
    cycles = character(),
    max_depth = 0
  )
  
  if (!"inheritances" %in% names(template_data)) {
    result$valid <- FALSE
    result$errors <- c(result$errors, "Template must include an 'inheritances' section (can be empty)")
    return(result)
  }
  
  inherit_map <- template_data$inheritances
  
  # Validate type: must be NULL or a list
  if (!is.null(inherit_map) && !is.list(inherit_map)) {
    result$valid <- FALSE
    result$errors <- c(result$errors, "Inherit section must be a named list")
    return(result)
  }
  
  # If no inheritances defined, nothing more to validate
  if (is.null(inherit_map) || length(inherit_map) == 0) {
    # Return valid result since empty inheritances is allowed
    return(result)
  }
  
  # Get all sections (excluding metadata sections)
  metadata_sections <- .get_metadata_sections()
  data_sections <- setdiff(names(template_data), metadata_sections)
  
  # Check that all inheritance targets exist
  for (section in names(inherit_map)) {
    parent <- inherit_map[[section]]
    
    # Skip NULL inheritance (no parent)
    if (is.null(parent)) next
    
    # Check section exists
    if (!section %in% data_sections) {
      result$warnings <- c(result$warnings,
                          paste0("Inheritance defined for non-existent section: '", section, "'"))
    }
    
    # Check parent exists
    if (!is.null(parent) && !parent %in% data_sections) {
      result$valid <- FALSE
      result$errors <- c(result$errors,
                        paste0("Section '", section, "' inherits from non-existent section: '", parent, "'"))
    }
  }
  
  # Build dependency graph and check for cycles
  result$graph <- inherit_map
  
  # Check each section for circular dependencies and depth
  for (section in names(inherit_map)) {
    parent <- inherit_map[[section]]
    
    # Skip NULL inheritance
    if (is.null(parent)) next
    
    # Check for self-inheritance (special case)
    if (section == parent) {
      result$valid <- FALSE
      result$cycles <- c(result$cycles, paste0(section, " -> ", section))
      result$errors <- c(result$errors,
                        paste0("Section '", section, "' cannot inherit from itself"))
      next
    }
    
    # Check if this relationship exists in a cycle
    # We need to trace the full path for error reporting
    visited <- character()
    current <- section
    depth <- 0
    
    while (!is.null(current) && current %in% names(inherit_map)) {
      if (current %in% visited) {
        # Circular dependency detected - extract the cycle
        cycle_path <- c(visited[seq(which(visited == current), length(visited))], current)
        result$valid <- FALSE
        result$cycles <- c(result$cycles, paste(cycle_path, collapse = " -> "))
        result$errors <- c(result$errors,
                          paste0("Circular inheritance detected: ", 
                                paste(cycle_path, collapse = " -> ")))
        break
      }
      
      visited <- c(visited, current)
      current <- inherit_map[[current]]
      depth <- depth + 1
      
      if (depth > max_depth) {
        result$warnings <- c(result$warnings,
                           paste0("Inheritance depth of ", depth, " for section '", section, 
                                 "' exceeds recommended maximum (", max_depth, ")"))
        result$max_depth <- max(result$max_depth, depth)
        break
      }
    }
    
    result$max_depth <- max(result$max_depth, depth)
  }
  
  return(result)
}


#' Validate Template Structure
#'
#' Validates the overall structure of the template including required sections
#' and proper naming conventions.
#'
#' @param template_data The parsed YAML template data
#' @return List with validation results
#' @keywords internal
.validate_template_structure <- function(template_data) {
  result <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    sections = character(),
    variables = character(),
    has_default = FALSE,
    yaml_valid = TRUE
  )
  
  # Check for at least one data section
  metadata_sections <- .get_metadata_sections()
  data_sections <- setdiff(names(template_data), metadata_sections)
  result$sections <- data_sections
  
  if (length(data_sections) == 0) {
    result$valid <- FALSE
    result$errors <- c(result$errors, "Template must contain at least one data section")
    return(result)
  }
  
  # Check for default section (recommended but not required)
  result$has_default <- "default" %in% data_sections
  if (!result$has_default) {
    result$warnings <- c(result$warnings, 
                        "Template does not contain a 'default' section (recommended)")
  }
  
  # Validate section names
  for (section in names(template_data)) {
    # Check for invalid characters in section names
    if (grepl("[^a-zA-Z0-9_.-]", section)) {
      result$warnings <- c(result$warnings,
                          paste0("Section name '", section, 
                                "' contains special characters (recommended: alphanumeric, underscore, dash, dot)"))
    }
  }
  
  # Collect all variables from data sections
  all_vars <- character()
  for (section in data_sections) {
    if (is.list(template_data[[section]])) {
      vars <- names(template_data[[section]])
      all_vars <- unique(c(all_vars, vars))
      
      # Validate variable names
      for (var in vars) {
        if (grepl("[^A-Z0-9_]", var)) {
          result$warnings <- c(result$warnings,
                             paste0("Variable name '", var, 
                                   "' should use UPPER_CASE with underscores"))
        }
      }
    }
  }
  result$variables <- all_vars
  
  return(result)
}


#' Validate Template Variable Consistency
#'
#' Checks consistency between variables in data sections and metadata sections.
#'
#' @param template_data The parsed YAML template data
#' @return List with validation results
#' @keywords internal
.validate_template_variables <- function(template_data) {
  result <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    orphaned_metadata = character(),
    missing_metadata = character()
  )
  
  # Get all variables from data sections
  metadata_sections <- .get_metadata_sections()
  data_sections <- setdiff(names(template_data), metadata_sections)
  
  all_data_vars <- character()
  for (section in data_sections) {
    if (is.list(template_data[[section]])) {
      all_data_vars <- unique(c(all_data_vars, names(template_data[[section]])))
    }
  }
  
  # Check descriptions
  if ("descriptions" %in% names(template_data)) {
    desc_vars <- names(template_data$descriptions)
    
    # Find orphaned descriptions (descriptions for non-existent variables)
    orphaned <- setdiff(desc_vars, all_data_vars)
    if (length(orphaned) > 0) {
      result$orphaned_metadata <- c(result$orphaned_metadata, orphaned)
      result$warnings <- c(result$warnings,
                          paste0("Descriptions exist for non-existent variables: ",
                                paste(orphaned, collapse = ", ")))
    }
    
    # Find missing descriptions (variables without descriptions)
    missing <- setdiff(all_data_vars, desc_vars)
    if (length(missing) > 0) {
      result$missing_metadata <- c(result$missing_metadata, missing)
      # This is info, not warning - descriptions are optional
      if (length(missing) <= 3) {
        result$warnings <- c(result$warnings,
                           paste0("Variables without descriptions: ",
                                 paste(missing, collapse = ", ")))
      } else {
        result$warnings <- c(result$warnings,
                           paste0(length(missing), " variables lack descriptions"))
      }
    }
  }
  
  # Check types
  if ("types" %in% names(template_data)) {
    type_vars <- names(template_data$types)
    
    # Find orphaned types
    orphaned <- setdiff(type_vars, all_data_vars)
    if (length(orphaned) > 0) {
      result$orphaned_metadata <- unique(c(result$orphaned_metadata, orphaned))
      result$warnings <- c(result$warnings,
                          paste0("Types defined for non-existent variables: ",
                                paste(orphaned, collapse = ", ")))
    }
  }
  
  # Check notes
  if ("notes" %in% names(template_data)) {
    note_vars <- names(template_data$notes)
    
    # Find orphaned notes
    orphaned <- setdiff(note_vars, all_data_vars)
    if (length(orphaned) > 0) {
      result$orphaned_metadata <- unique(c(result$orphaned_metadata, orphaned))
      result$warnings <- c(result$warnings,
                          paste0("Notes exist for non-existent variables: ",
                                paste(orphaned, collapse = ", ")))
    }
  }
  
  # Check options
  if ("options" %in% names(template_data)) {
    option_vars <- names(template_data$options)
    
    # Find orphaned options
    orphaned <- setdiff(option_vars, all_data_vars)
    if (length(orphaned) > 0) {
      result$orphaned_metadata <- unique(c(result$orphaned_metadata, orphaned))
      result$warnings <- c(result$warnings,
                          paste0("Options defined for non-existent variables: ",
                                paste(orphaned, collapse = ", ")))
    }
  }
  
  return(result)
}


#' Validate Template Types
#'
#' Validates that values match their declared types across all sections.
#'
#' @param template_data The parsed YAML template data
#' @param package Package name for type normalization
#' @return List with validation results
#' @keywords internal
.validate_template_types <- function(template_data, package = NULL) {
  result <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    type_mismatches = list()
  )
  
  if (!"types" %in% names(template_data)) {
    return(result)
  }
  
  types_map <- template_data$types
  metadata_sections <- .get_metadata_sections()
  data_sections <- setdiff(names(template_data), metadata_sections)
  
  # Check each typed variable across all sections
  for (var_name in names(types_map)) {
    # Get declared type from already-loaded template and normalize it
    declared_type <- .normalize_type(types_map[[var_name]])
    
    # Skip if no type declared
    if (is.null(declared_type)) next
    
    for (section in data_sections) {
      if (!is.list(template_data[[section]]) || 
          !var_name %in% names(template_data[[section]])) {
        next
      }
      
      value <- template_data[[section]][[var_name]]
      
      # Skip NULL values
      if (is.null(value)) next
      
      # Check type match
      actual_type <- .detect_variable_type(value)
      
      # Simple comparison - both types are already normalized
      if (declared_type != actual_type) {
        
        if (!var_name %in% names(result$type_mismatches)) {
          result$type_mismatches[[var_name]] <- list()
        }
        
        result$type_mismatches[[var_name]][[section]] <- list(
          declared = declared_type,
          actual = actual_type,
          value = as.character(value)
        )
        
        result$warnings <- c(result$warnings,
                           paste0("Variable '", var_name, "' in section '", section,
                                 "' has type '", actual_type, "' but declared as '",
                                 declared_type, "'"))
      }
      
      # Check options if specified
      if ("options" %in% names(template_data) && 
          var_name %in% names(template_data$options)) {
        allowed_options <- template_data$options[[var_name]]
        
        if (!is.null(value) && !value %in% allowed_options) {
          result$warnings <- c(result$warnings,
                             paste0("Variable '", var_name, "' in section '", section,
                                   "' has value '", value, "' not in allowed options: ",
                                   paste(allowed_options, collapse = ", ")))
        }
      }
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