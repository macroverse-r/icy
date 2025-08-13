#' qconfig Type Conversion Utilities
#'
#' Internal functions for handling type conversion and data transformation
#' in the qconfig interactive configuration system.
#'
#' @name qconfig-type-utils
#' @keywords internal
NULL

#' Convert Return Value to Proper Type
#'
#' Internal helper function to convert string values to proper R types.
#'
#' @param value Raw value (character string or NULL)
#' @param type Expected type
#' @return Converted value with proper R type
#' @keywords internal
.convert_return_value <- function(value, type) {
  # Return NULL as-is
  if (is.null(value)) {
    return(invisible(NULL))
  }
  
  # Convert based on type
  if (is.null(type)) {
    return(invisible(value))  # No type specified, return as-is
  }
  
  converted_value <- switch(type,
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
      if (value %in% c("yes", "true", "TRUE", "True", "on", "1")) {
        TRUE
      } else if (value %in% c("no", "false", "FALSE", "False", "off", "0")) {
        FALSE
      } else {
        value  # Return original if can't convert
      }
    },
    # Default: return as-is
    value
  )
  
  return(invisible(converted_value))
}

#' Validate Input Type
#'
#' Validates that a string value can be properly converted to the specified type.
#' Returns an error message if validation fails, or NULL if validation passes.
#'
#' @param value Value to validate
#' @param type Target type ("character", "integer", "numeric", "logical", or NULL)
#' @return NULL if valid, error message string if invalid
#' @keywords internal
.validate_input_type <- function(value, type) {
  # NULL type means no validation needed
  if (is.null(type)) {
    return(NULL)
  }
  
  # Character type accepts anything
  if (type == "character") {
    return(NULL)
  }
  
  # Validate based on type
  switch(type,
    "integer" = {
      # Convert to numeric first to check if it's a valid number
      numeric_val <- suppressWarnings(as.numeric(value))
      if (is.na(numeric_val)) {
        return(paste0("'", value, "' is not a valid integer"))
      }
      # Check if it's a whole number (no decimal part)
      if (numeric_val != as.integer(numeric_val)) {
        return(paste0("'", value, "' is not a valid integer (decimals not allowed)"))
      }
      NULL
    },
    "numeric" = {
      converted <- suppressWarnings(as.numeric(value))
      if (is.na(converted)) {
        return(paste0("'", value, "' is not a valid number"))
      }
      NULL
    },
    "logical" = {
      if (!value %in% c("yes", "true", "TRUE", "True", "on", "1", 
                        "no", "false", "FALSE", "False", "off", "0")) {
        return(paste0("'", value, "' is not a valid boolean value. Use: TRUE/FALSE, yes/no, 1/0, true/false, on/off"))
      }
      NULL
    },
    # Default: no validation for unknown types
    NULL
  )
}

#' Convert Value by Type
#'
#' Converts a string value to the specified type for proper YAML representation.
#'
#' @param value Value to convert
#' @param type Target type ("character", "integer", "numeric", "logical", or NULL)
#' @return Converted value
#' @keywords internal
.convert_by_type <- function(value, type) {
  # Handle NULL type - return value as-is
  if (is.null(type)) {
    return(value)
  }
  
  # Convert based on specified type
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
      if (value %in% c("yes", "true", "TRUE", "True", "on", "1")) {
        TRUE
      } else if (value %in% c("no", "false", "FALSE", "False", "off", "0")) {
        FALSE
      } else {
        value  # Return original if can't convert
      }
    },
    # Default: return as-is
    value
  )
}