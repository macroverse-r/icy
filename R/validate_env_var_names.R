#' Validate Environment Variable Names
#'
#' Checks that all provided environment variable names are in the allowed list
#' defined in the YAML configuration file.
#'
#' @param var_names Character vector of variable names to validate.
#' @param warn Logical; if TRUE, issues a warning for unknown variables instead of an error.
#' @param allowed_vars Character vector of allowed variable names. If NULL (default),
#'   the function will retrieve the list using `get_env_var_names()`.
#' 
#' @return Logical; TRUE if all variables are valid, otherwise issues a warning or error.
#' @export
validate_env_var_names <- function(var_names, warn = TRUE, allowed_vars = NULL) {
  if (is.null(allowed_vars)) {
    allowed_vars <- get_env_var_names()
  }
  
  unknown_vars <- setdiff(var_names, allowed_vars)
  
  if (length(unknown_vars) > 0) {
    msg <- paste("The following variables are not in the standard environment variables list:",
                 paste(unknown_vars, collapse = ", "))
    
    if (warn) {
      cli::cli_warn(msg)
      return(FALSE)
    } else {
      cli::cli_abort(msg)
    }
  }
  
  return(TRUE)
}