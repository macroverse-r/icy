#' Validate Template Configuration File
#'
#' Performs comprehensive validation of template YAML files including structure,
#' inheritance chains, variable consistency, and metadata integrity.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()`.
#' @param fn_tmpl Character string with custom filename for the template.
#'   If NULL, uses the default naming pattern based on case_format.
#' @param case_format Character string indicating the case format to use for
#'   searching the template file. Options are: "snake_case" (default), "camelCase", 
#'   "PascalCase", "kebab-case".
#' @param verbose Logical. If TRUE (default), displays validation progress and results.
#' @param quick Logical. If FALSE (default), performs complete validation. If TRUE,
#'   only performs critical checks for performance.
#'
#' @return An object of class "icy_validation_result" containing:
#'   - `valid`: Overall validation status (TRUE/FALSE)
#'   - `errors`: Character vector of error messages
#'   - `warnings`: Character vector of warning messages
#'   - `info`: Character vector of informational messages
#'   - `inheritance`: Detailed inheritance validation results
#'   - `structure`: Template structure information
#'   - `consistency`: Variable consistency check results
#'
#' @examples
#' \dontrun{
#' # Validate current package template
#' result <- validate_template()
#' 
#' # Validate specific package
#' result <- validate_template("mypackage")
#' 
#' # Validate custom template file
#' result <- validate_template(fn_tmpl = "my_custom_template")
#' 
#' # Quick validation (only critical checks)
#' result <- validate_template(quick = TRUE)
#' }
#'
#' @export
validate_template <- function(package = get_package_name(verbose = FALSE),
                            fn_tmpl = NULL,
                            case_format = "snake_case",
                            verbose = TRUE,
                            quick = FALSE) {
  
  # Initialize result structure
  result <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    info = character(),
    inheritance = list(valid = TRUE),
    structure = list(),
    consistency = list()
  )
  
  # Find template file
  template_file <- find_template(
    package = package,
    fn_tmpl = fn_tmpl,
    case_format = case_format
  )
  
  if (is.null(template_file)) {
    result$valid <- FALSE
    if (is.null(fn_tmpl)) {
      result$errors <- c(result$errors, 
                         paste0("No template configuration file found for package '", package, "'"))
    } else {
      result$errors <- c(result$errors, 
                         paste0("Template file not found: ", fn_tmpl))
    }
    return(structure(result, class = "icy_validation_result"))
  }
  
  result$info <- c(result$info, paste0("Validating template: ", template_file))
  
  # Read template file
  template_data <- tryCatch({
    yaml::read_yaml(template_file)
  }, error = function(e) {
    result$valid <<- FALSE
    result$errors <<- c(result$errors, 
                        paste0("Invalid YAML syntax: ", e$message))
    return(NULL)
  })
  
  if (is.null(template_data)) {
    return(structure(result, class = "icy_validation_result"))
  }
  
  # Perform structure validation
  structure_result <- .validate_template_structure(template_data)
  result$structure <- structure_result
  if (!structure_result$valid) {
    result$valid <- FALSE
    result$errors <- c(result$errors, structure_result$errors)
  }
  result$warnings <- c(result$warnings, structure_result$warnings)
  
  # Perform inheritance validation (always do this, even in quick mode)
  if ("inheritances" %in% names(template_data)) {
    inheritance_result <- .validate_template_inheritance(template_data)
    result$inheritance <- inheritance_result
    if (!inheritance_result$valid) {
      result$valid <- FALSE
      result$errors <- c(result$errors, inheritance_result$errors)
    }
    result$warnings <- c(result$warnings, inheritance_result$warnings)
  }
  
  # Skip detailed validation in quick mode
  if (!quick) {
    # Validate variable consistency
    consistency_result <- .validate_template_variables(template_data)
    result$consistency <- consistency_result
    if (!consistency_result$valid) {
      result$valid <- FALSE
      result$errors <- c(result$errors, consistency_result$errors)
    }
    result$warnings <- c(result$warnings, consistency_result$warnings)
    
    # Validate types if present
    if ("types" %in% names(template_data)) {
      types_result <- .validate_template_types(template_data, package)
      if (!types_result$valid) {
        result$valid <- FALSE
        result$errors <- c(result$errors, types_result$errors)
      }
      result$warnings <- c(result$warnings, types_result$warnings)
    }
  }
  
  # Return result with class for auto-printing
  return(structure(result, class = "icy_validation_result"))
}


#' Validate Template Inheritance
#'
#' Checks for circular dependencies and validates inheritance chains in templates.
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
    return(result)
  }
  
  inherit_map <- template_data$inheritances
  if (!is.list(inherit_map)) {
    result$valid <- FALSE
    result$errors <- c(result$errors, "Inherit section must be a named list")
    return(result)
  }
  
  # Get all sections (excluding metadata sections)
  metadata_sections <- c("types", "descriptions", "notes", "options", "inheritances")
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
  
  # Check each section for circular dependencies
  for (section in names(inherit_map)) {
    visited <- character()
    current <- section
    depth <- 0
    
    while (!is.null(current) && current %in% names(inherit_map)) {
      if (current %in% visited) {
        # Circular dependency detected
        cycle_path <- c(visited[seq(which(visited == current), length(visited))], current)
        result$valid <- FALSE
        result$cycles <- c(result$cycles, paste(cycle_path, collapse = " → "))
        result$errors <- c(result$errors,
                          paste0("Circular inheritance detected: ", 
                                paste(cycle_path, collapse = " → ")))
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
  metadata_sections <- c("types", "descriptions", "notes", "options", "inheritances")
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
  metadata_sections <- c("types", "descriptions", "notes", "options", "inheritances")
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
  metadata_sections <- c("types", "descriptions", "notes", "options", "inheritances")
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


#' Print Method for Template Validation Results
#'
#' Pretty prints the validation results with colors and formatting.
#'
#' @param x An object of class "icy_validation_result"
#' @param ... Additional arguments (unused)
#' @return Invisible NULL
#' @export
print.icy_validation_result <- function(x, ...) {
  # Overall status
  if (x$valid) {
    .icy_success("VALID")
  } else {
    .icy_alert(paste0("INVALID - ", length(x$errors), " error(s)"))
  }
  
  # Display errors
  if (length(x$errors) > 0) {
    .icy_text("")
    .icy_text(.apply_color("ERRORS:", "red"))
    for (error in x$errors) {
      .icy_text(paste0("  • ", error))
    }
  }
  
  # Display warnings
  if (length(x$warnings) > 0) {
    .icy_text("")
    .icy_text(.apply_color("WARNINGS:", "yellow"))
    for (warning in x$warnings) {
      .icy_text(paste0("  • ", warning))
    }
  }
  
  # Display info
  if (length(x$info) > 0 && length(x$errors) == 0 && length(x$warnings) == 0) {
    .icy_text("")
    .icy_text(.apply_color("INFO:", "gray"))
    for (info in x$info) {
      .icy_text(paste0("  i ", info))
    }
  }
  
  # Summary statistics
  if (!is.null(x$structure)) {
    .icy_text("")
    summary_parts <- c()
    
    if (length(x$structure$sections) > 0) {
      summary_parts <- c(summary_parts, 
                        paste0(length(x$structure$sections), " section(s)"))
    }
    
    if (length(x$structure$variables) > 0) {
      summary_parts <- c(summary_parts,
                        paste0(length(x$structure$variables), " variable(s)"))
    }
    
    if (length(summary_parts) > 0) {
      .icy_text(paste0("Template structure: ", paste(summary_parts, collapse = ", ")))
    }
  }
  
  # Inheritance info
  if (!is.null(x$inheritance) && !is.null(x$inheritance$max_depth) && 
      x$inheritance$max_depth > 0) {
    .icy_text(paste0("Longest inheritance chain: ", x$inheritance$max_depth, " levels"))
  }
  
  invisible(NULL)
}
