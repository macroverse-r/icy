


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
    # Check if it can be represented as integer without overflow
    int_val <- suppressWarnings(as.integer(value))
    if (!is.na(int_val) && value == int_val) {
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




#' Convert Value to Specified Type
#'
#' Converts a value to the specified type. Used for type consistency
#' when merging configurations from different sources (YAML, environment variables, etc.).
#'
#' @param value Value to convert
#' @param type Target type: "character", "integer", "numeric", "logical", or "path"
#' @return Converted value, or original value if conversion fails
#' @keywords internal
.convert_by_type <- function(value, type) {
  if (is.null(type)) {
    return(value)
  }
  
  switch(type,
    "character" = as.character(value),
    "integer" = {
      converted <- suppressWarnings(as.integer(value))
      if (is.na(converted)) value else converted
    },
    "numeric" = {
      converted <- suppressWarnings(as.numeric(value))
      if (is.na(converted)) value else converted
    },
    "logical" = {
      if (is.na(value) || value == "") {
        return(NA)
      }
      
      lower_value <- tolower(trimws(value))
      
      if (lower_value %in% c("yes", "true", "t", "y", "on", "1")) {
        TRUE
      } else if (lower_value %in% c("no", "false", "f", "n", "off", "0")) {
        FALSE
      } else {
        value
      }
    },
    "path" = as.character(value),
    value  # Return unchanged for unknown types
  )
}

#' Resolve Special Path Keywords
#'
#' Resolves special keywords in path strings to actual paths.
#' Supports keywords like "home", "tempdir", "getwd" and path composition with pipe separator.
#' Also resolves variable references like ${VAR_NAME} when config is provided.
#'
#' @param path_string Path string potentially containing keywords or variable references
#' @param package Package name for package-specific directories
#' @param config Optional named list of configuration for resolving ${VAR_NAME} references
#' @return Resolved path string
#' @keywords internal
.resolve_special_path <- function(path_string, package = NULL, config = NULL) {
  # First resolve variable references if config is provided
  if (!is.null(config) && is.character(path_string)) {
    pattern <- "\\$\\{([A-Z_][A-Z0-9_]*)\\}"
    matches <- gregexpr(pattern, path_string, perl = TRUE)
    
    if (matches[[1]][1] != -1) {
      # Extract matched variable names
      match_starts <- matches[[1]]
      match_lengths <- attr(matches[[1]], "match.length")
      capture_starts <- attr(matches[[1]], "capture.start")
      capture_lengths <- attr(matches[[1]], "capture.length")
      
      # Process from end to beginning to maintain positions
      for (i in length(match_starts):1) {
        full_match <- substr(path_string, match_starts[i], 
                           match_starts[i] + match_lengths[i] - 1)
        ref_var_name <- substr(path_string, capture_starts[i], 
                             capture_starts[i] + capture_lengths[i] - 1)
        
        # Check if referenced variable exists in config
        if (ref_var_name %in% names(config)) {
          ref_value <- config[[ref_var_name]]
          # Replace the reference with the resolved value
          path_string <- sub(full_match, ref_value, path_string, fixed = TRUE)
        }
      }
    }
  }
  
  .resolve_keyword <- function(keyword) {
    switch(keyword,
      "home" = path.expand("~"),
      "cache" = tools::R_user_dir(package, "cache"),
      "config" = tools::R_user_dir(package, "config"), 
      "data" = tools::R_user_dir(package, "data"),
      "tempdir" = tempdir(),
      "getwd" = getwd(),
      "." = getwd(),
      ".." = dirname(getwd()),
      keyword
    )
  }
  
  if (grepl("[|/\\\\]", path_string)) {
    if (grepl("\\|", path_string)) {
      parts <- strsplit(path_string, "\\|")[[1]]
    } else if (grepl("/", path_string)) {
      parts <- strsplit(path_string, "/")[[1]]
    } else if (grepl("\\\\", path_string)) {
      parts <- strsplit(path_string, "\\\\")[[1]]
    }
    
    base_path <- .resolve_keyword(parts[1])
    if (length(parts) > 1) {
      return(do.call(file.path, c(list(base_path), parts[-1])))
    } else {
      return(base_path)
    }
  }
  
  return(.resolve_keyword(path_string))
}

#' Resolve Variable References in Configuration
#'
#' Resolves ${VAR_NAME} patterns in configuration values by substituting them
#' with actual values from the configuration. Handles nested references and
#' detects circular dependencies.
#'
#' @param config Named list of configuration variables
#' @param visited Character vector of variable names being resolved (for circular detection)
#' @param max_depth Maximum recursion depth to prevent infinite loops
#' @return Configuration list with all variable references resolved
#' @keywords internal
.resolve_variable_references <- function(config, visited = character(), max_depth = 10) {
  if (length(config) == 0) {
    return(config)
  }
  
  if (length(visited) >= max_depth) {
    stop("Maximum variable reference depth exceeded. Check for circular references.")
  }
  
  resolved_config <- config
  
  for (var_name in names(config)) {
    value <- config[[var_name]]
    
    # Only process character values that might contain references
    if (!is.character(value) || is.na(value) || is.null(value)) {
      next
    }
    
    # Check if this variable is currently being resolved (circular reference)
    if (var_name %in% visited) {
      warning(paste0("Circular reference detected for variable '", var_name, 
                     "'. Using original value."))
      next
    }
    
    # Find all ${VAR_NAME} patterns
    pattern <- "\\$\\{([A-Z_][A-Z0-9_]*)\\}"
    matches <- gregexpr(pattern, value, perl = TRUE)
    
    if (matches[[1]][1] != -1) {
      # Extract matched variable names
      match_starts <- matches[[1]]
      match_lengths <- attr(matches[[1]], "match.length")
      capture_starts <- attr(matches[[1]], "capture.start")
      capture_lengths <- attr(matches[[1]], "capture.length")
      
      # Process from end to beginning to maintain positions
      for (i in length(match_starts):1) {
        full_match <- substr(value, match_starts[i], 
                           match_starts[i] + match_lengths[i] - 1)
        ref_var_name <- substr(value, capture_starts[i], 
                             capture_starts[i] + capture_lengths[i] - 1)
        
        # Check if referenced variable exists
        if (ref_var_name %in% names(config)) {
          # Get the referenced value
          ref_value <- config[[ref_var_name]]
          
          # If the referenced value also contains references, resolve them first
          if (is.character(ref_value) && grepl(pattern, ref_value)) {
            # Check if we would create a circular reference
            if (ref_var_name %in% c(visited, var_name)) {
              warning(paste0("Circular reference detected: ", var_name, " -> ", ref_var_name, ". Using original value."))
            } else {
              # Recursively resolve the referenced variable with full config context
              temp_config <- list()
              temp_config[[ref_var_name]] <- ref_value
              # Add all other variables for context in recursive resolution
              for (other_var in names(config)) {
                if (other_var != ref_var_name) {
                  temp_config[[other_var]] <- config[[other_var]]
                }
              }
              resolved_temp <- .resolve_variable_references(
                temp_config,
                c(visited, var_name),
                max_depth
              )
              ref_value <- resolved_temp[[ref_var_name]]
            }
          }
          
          # Replace the reference with the resolved value
          value <- sub(full_match, ref_value, value, fixed = TRUE)
        } else {
          warning(paste0("Variable '", ref_var_name, 
                        "' referenced in '", var_name, 
                        "' but not defined. Keeping reference as-is."))
        }
      }
      
      resolved_config[[var_name]] <- value
    }
  }
  
  return(resolved_config)
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
    # Try integer first, but check for overflow
    int_val <- suppressWarnings(as.integer(value_str))
    if (!is.na(int_val)) {
      return(int_val)
    }
    # If integer conversion failed (overflow), try numeric
    return(as.numeric(value_str))
  }
  if (grepl("^-?[0-9]*\\.[0-9]+$", value_str)) {
    return(as.numeric(value_str))
  }
  
  # Return as character
  return(value_str)
}