#' qconfig Input Processing Utilities
#'
#' Internal functions for parsing and processing user input in the qconfig 
#' interactive configuration system, including suffix notation and skip handling.
#'
#' @name qconfig-input-utils
#' @keywords internal
NULL

#' Parse Selection Input with Resolution Mode Suffixes
#'
#' Internal helper function to parse user input that may contain resolution
#' mode suffixes (s for static, d for dynamic).
#'
#' @param input Character string with user input (e.g., "8", "7s", "3d")
#' @return List with selection number and resolution mode
#' @keywords internal
.parse_selection_input <- function(input) {
  input <- trimws(input)
  
  # Check for suffix patterns (number followed by s or d)
  if (grepl("^[0-9]+[sd]$", input)) {
    number <- as.integer(gsub("[sd]$", "", input))
    mode <- if (endsWith(input, "s")) "static" else "dynamic"
    return(list(selection = number, mode = mode))
  }
  
  # Regular selection number - will ask if needed
  number <- suppressWarnings(as.integer(input))
  return(list(selection = number, mode = "ask"))
}

#' Handle Skip Input
#'
#' Internal helper function to handle skip logic consistently.
#'
#' @param user_input User input string
#' @param allow_skip Whether skipping is allowed
#' @return NULL if skipped, FALSE if not skipped
#' @keywords internal
.handle_skip_input <- function(user_input, allow_skip) {
  if (allow_skip && nchar(user_input) == 0) {
    .icy_inform("Skipped configuration")
    return(NULL)
  }
  return(FALSE)  # Not skipped
}

#' Determine Allow Custom Setting
#'
#' Internal helper function to determine if custom input should be allowed
#' based on variable type when allow_custom is NULL.
#'
#' @param type Variable type (e.g., "logical", "path", "character", etc.)
#' @param allow_custom User-specified allow_custom parameter
#' @return Logical value indicating whether custom input should be allowed
#' @keywords internal
.determine_allow_custom <- function(type, allow_custom) {
  # If explicitly set, use that value
  if (!is.null(allow_custom)) {
    return(allow_custom)
  }
  
  # Smart defaults based on type
  if (is.null(type)) {
    return(TRUE)  # Default when no type information
  }
  
  switch(type,
    "logical" = FALSE,  # Boolean values should be TRUE/FALSE only
    "path" = TRUE,      # Users often need custom directory paths  
    "character" = TRUE, # Users may need custom text input
    "integer" = TRUE,   # Users may need custom numeric values
    "numeric" = TRUE,   # Users may need custom numeric values
    TRUE                # Default to allowing custom input
  )
}