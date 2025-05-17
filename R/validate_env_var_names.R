#' Validate Environment Variable Names Against Allowed List
#'
#' Ensures that all provided environment variable names are in the allowed list defined
#' in the package's YAML configuration file. This function provides a safeguard against
#' typos and ensures consistent variable naming.
#'
#' This validation function is primarily used internally by other functions in the package,
#' but can also be used directly in package development to verify environment variable
#' names before using them.
#'
#' The function will either warn or error (depending on the `warn` parameter) if any
#' environment variable names are not in the allowed list.
#'
#' @param var_names Character vector of variable names to validate.
#' @param package Character string with the package name to retrieve the allowed list from.
#' @param warn Logical; if TRUE (default), issues a warning for unknown variables.
#'   If FALSE, raises an error for unknown variables.
#' @param allowed_vars Optional character vector of allowed variable names. If NULL (default),
#'   the function will retrieve the list from the package's YAML config using `get_env_var_names()`.
#' 
#' @return Logical; TRUE if all variables are valid, FALSE if any are invalid (when warn=TRUE),
#'   or an error (when warn=FALSE).
#'
#' @examples
#' \dontrun{
#' # Validate a list of variable names against the package's YAML config
#' valid <- validate_env_var_names(
#'   c("MY_PACKAGE_API_KEY", "MY_PACKAGE_DATA_DIR"), 
#'   package = "mypackage"
#' )
#' 
#' # With a custom list of allowed variables
#' allowed <- c("MY_PACKAGE_API_KEY", "MY_PACKAGE_DATA_DIR", "MY_PACKAGE_DEBUG")
#' valid <- validate_env_var_names(
#'   c("MY_PACKAGE_API_KEY", "MY_PACKAGE_DEBUG"),
#'   package = "mypackage",
#'   allowed_vars = allowed
#' )
#' 
#' # Error on invalid variables instead of warning
#' tryCatch({
#'   validate_env_var_names(
#'     c("MY_PACKAGE_API_KEY", "TYPO_IN_VAR_NAME"),
#'     package = "mypackage",
#'     warn = FALSE
#'   )
#' }, error = function(e) {
#'   cat("Validation failed:", e$message, "\n")
#' })
#' }
#'
#' @keywords internal
validate_env_var_names <- function(var_names, package, warn = TRUE, allowed_vars = NULL) {

  if (is.null(allowed_vars)) {
    allowed_vars <- get_env_var_names(package = package)
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
