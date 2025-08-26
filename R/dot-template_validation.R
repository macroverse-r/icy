


#' Validate Variable Name
#'
#' Validates a variable name according to naming conventions.
#'
#' @param var_name Variable name to validate
#' @param package Package name for prefix checking
#' @param existing_vars Character vector of existing variable names
#' @param require_prefix Logical; if TRUE, requires package prefix
#' @param auto_clean Logical; if TRUE, returns cleaned name instead of error
#' @return List with 'valid', 'name', and 'message' components
#' @keywords internal
.validate_variable_name <- function(var_name, 
                                   package = NULL,
                                   existing_vars = character(0),
                                   require_prefix = FALSE,
                                   auto_clean = TRUE) {
  
  # Basic validation
  if (is.null(var_name) || nchar(trimws(var_name)) == 0) {
    return(list(
      valid = FALSE,
      name = NULL,
      message = "Variable name cannot be empty"
    ))
  }
  
  original_name <- var_name
  var_name <- trimws(var_name)
  
  # Auto-clean if requested
  if (auto_clean) {
    # Convert to uppercase and replace invalid characters
    var_name <- toupper(var_name)
    var_name <- gsub("[^A-Z0-9_]", "_", var_name)
    var_name <- gsub("_+", "_", var_name)  # Collapse multiple underscores
    var_name <- gsub("^_|_$", "", var_name)  # Remove leading/trailing underscores
  }
  
  # Check format
  if (!grepl("^[A-Z][A-Z0-9_]*$", var_name)) {
    return(list(
      valid = FALSE,
      name = var_name,
      message = "Variable name must contain only uppercase letters, numbers, and underscores"
    ))
  }
  
  # Check package prefix if required
  if (require_prefix && !is.null(package)) {
    package_upper <- toupper(package)
    if (!startsWith(var_name, paste0(package_upper, "_"))) {
      if (auto_clean) {
        # Add prefix
        var_name <- paste0(package_upper, "_", var_name)
      } else {
        return(list(
          valid = FALSE,
          name = var_name,
          message = paste0("Variable name must start with ", package_upper, "_")
        ))
      }
    }
  }
  
  # Check for duplicates
  if (var_name %in% existing_vars) {
    return(list(
      valid = FALSE,
      name = var_name,
      message = paste0("Variable '", var_name, "' already exists")
    ))
  }
  
  # Return result
  return(list(
    valid = TRUE,
    name = var_name,
    message = if (var_name != original_name) {
      paste0("Name cleaned from '", original_name, "' to '", var_name, "'")
    } else {
      ""
    }
  ))
}


#' Validate Variable Type
#'
#' Validates that a value matches the expected type.
#'
#' @param value Value to validate
#' @param expected_type Expected type (character, logical, integer, numeric, path)
#' @param var_name Variable name for error messages
#' @return List with 'valid' and 'message' components
#' @keywords internal
.validate_variable_type <- function(value, expected_type, var_name = "value") {
  if (is.null(expected_type) || is.na(expected_type)) {
    return(list(valid = TRUE, message = ""))
  }
  
  # Handle NULL specially
  if (is.null(value)) {
    return(list(valid = TRUE, message = ""))
  }
  
  valid <- switch(expected_type,
    "logical" = is.logical(value) || 
                (is.character(value) && tolower(value) %in% c("true", "false", "yes", "no", "y", "n")),
    "integer" = is.numeric(value) && (value == as.integer(value)) ||
                (is.character(value) && !is.na(suppressWarnings(as.integer(value)))),
    "numeric" = is.numeric(value) ||
                (is.character(value) && !is.na(suppressWarnings(as.numeric(value)))),
    "path" = is.character(value),
    "character" = TRUE,  # Everything can be character
    TRUE  # Unknown type, allow
  )
  
  if (!valid) {
    return(list(
      valid = FALSE,
      message = paste0(
        "Variable '", var_name, "' expects type '", expected_type,
        "' but got '", class(value)[1], "'"
      )
    ))
  }
  
  return(list(valid = TRUE, message = ""))
}


#' Detect Variable Type
#'
#' Auto-detects the type of a value.
#'
#' @param value Value to analyze
#' @return Character string indicating detected type
#' @keywords internal
.detect_variable_type <- function(value) {
  # Handle NULL
  if (is.null(value)) {
    return("character")  # Default for NULL
  }
  
  # Check logical first (most specific)
  if (is.logical(value)) {
    return("logical")
  }
  
  # Check numeric types
  if (is.numeric(value)) {
    if (value == as.integer(value)) {
      return("integer")
    } else {
      return("numeric")
    }
  }
  
  # Check character patterns
  if (is.character(value)) {
    # Check for boolean patterns
    if (tolower(value) %in% c("true", "false", "yes", "no", "y", "n")) {
      return("logical")
    }
    
    # Check for numeric patterns
    if (!is.na(suppressWarnings(as.numeric(value)))) {
      if (grepl("^-?[0-9]+$", value)) {
        return("integer")
      } else {
        return("numeric")
      }
    }
    
    # Check for path patterns
    if (grepl("^(/|~|\\./|\\.\\./).*", value) || 
        grepl("\\\\|/", value)) {
      return("path")
    }
  }
  
  # Default to character
  return("character")
}


#' Validate Variables Against Template
#'
#' Validates a set of variables against a template section.
#'
#' @param var_list List of variables to validate
#' @param template_data Template data structure
#' @param section Section to validate against
#' @param check_types Logical; if TRUE, validates types
#' @return List with 'valid', 'invalid_vars', and 'message' components
#' @keywords internal
.validate_variables_against_template <- function(var_list, 
                                                template_data,
                                                section = "default",
                                                check_types = TRUE) {
  
  # Get template variables
  if (section %in% names(template_data)) {
    template_vars <- names(template_data[[section]])
  } else {
    return(list(
      valid = FALSE,
      invalid_vars = names(var_list),
      message = paste0("Section '", section, "' not found in template")
    ))
  }
  
  # Check for unknown variables
  invalid_vars <- setdiff(names(var_list), template_vars)
  if (length(invalid_vars) > 0) {
    return(list(
      valid = FALSE,
      invalid_vars = invalid_vars,
      message = paste0(
        "Unknown variable", if(length(invalid_vars) > 1) "s" else "", ": ",
        paste(invalid_vars, collapse = ", ")
      )
    ))
  }
  
  # Check types if requested
  if (check_types && "types" %in% names(template_data)) {
    type_errors <- character(0)
    
    for (var_name in names(var_list)) {
      if (var_name %in% names(template_data$types)) {
        expected_type <- template_data$types[[var_name]]
        type_check <- .validate_variable_type(
          var_list[[var_name]], 
          expected_type,
          var_name
        )
        
        if (!type_check$valid) {
          type_errors <- c(type_errors, type_check$message)
        }
      }
    }
    
    if (length(type_errors) > 0) {
      return(list(
        valid = FALSE,
        invalid_vars = character(0),
        message = paste(type_errors, collapse = "\n")
      ))
    }
  }
  
  return(list(
    valid = TRUE,
    invalid_vars = character(0),
    message = ""
  ))
}


#' Parse Input Value
#'
#' Parses user input to appropriate R type.
#'
#' @param value_str Character string input
#' @return Parsed value
#' @keywords internal
.parse_input_value <- function(value_str) {
  value_str <- trimws(value_str)
  
  # Check for logical values
  lower_val <- tolower(value_str)
  if (lower_val %in% c("true", "t", "yes", "y")) return(TRUE)
  if (lower_val %in% c("false", "f", "no", "n")) return(FALSE)
  
  # Check for numeric values
  if (grepl("^-?[0-9]+$", value_str)) {
    return(as.integer(value_str))
  }
  if (grepl("^-?[0-9]*\\.[0-9]+$", value_str)) {
    return(as.numeric(value_str))
  }
  
  # Return as character
  return(value_str)
}