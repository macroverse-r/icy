#' Toggle or Initialize Debug Mode for Package
#'
#' @description
#' Toggles the debug mode setting for the specified package by modifying the 
#' `PKGNAME_DEBUG` variable in the package's environment variables YAML file.
#' If the variable doesn't exist, it will be created with the specified initial value.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param user Character string. The user configuration to modify. Defaults to "default".
#' @param initial Logical. The initial value to use when initializing a 
#'   non-existent debug variable. Defaults to TRUE.
#' @param verbose Logical. If TRUE, displays informative messages about the operation. If NULL (default), uses the package's VERBOSE configuration value, or FALSE if not set. 
#'
#' @return Invisibly returns the new value of the debug variable.
#'
#' @details
#' The function uses the provided package name (or determines it dynamically) 
#' and creates the appropriate debug variable name (`PKGNAME_DEBUG` where PKGNAME
#' is the uppercase package name). It reads the configuration from the local YAML file,
#' toggles or initializes the debug variable, and writes the updated configuration
#' back to the file.
#'
#' The function will display a success message unless the `PKGNAME_VERBOSE` variable
#' exists and is set to FALSE.
#'
#' @examples
#' \dontrun{
#' # Toggle debug mode for a specific package
#' toggle_debug(package = "mypackage")
#'
#' # Initialize debug mode with initial FALSE
#' toggle_debug(package = "otherpackage", initial = FALSE)
#' }
#'
#' @seealso
#' \code{\link{get_config}} for retrieving the config data.
#'
#' @export
toggle_debug <- function(package = get_package_name(),
                         user = "default",
                         initial = TRUE,
                         verbose = NULL) {
  
  # Create dynamic variable names based on package
  pkg_upper <- toupper(package)
  debug_var <- paste0(pkg_upper, "_DEBUG")
  verbose_var <- paste0(pkg_upper, "_VERBOSE")
  
  # Read current config
  current_config <- tryCatch({
    get_config(package = package, origin = "local", user = user)
  }, error = function(e) {
    list()
  })
  
  # Set verbose default from config if not explicitly provided
  if (is.null(verbose)) {
    verbose <- if (!is.null(current_config[[verbose_var]])) {
      as.logical(current_config[[verbose_var]])
    } else {
      FALSE
    }
  }
  
  # Check if debug variable exists
  if (is.null(current_config[[debug_var]])) {
    new_value <- initial
    msg <- " - initialized"
  } else {
    # Toggle existing value
    current_value <- as.logical(current_config[[debug_var]])
    new_value <- !current_value
    msg <- ""
  }
  
  # Write updated value to local config
  write_local(
    var_list = structure(list(new_value), names = debug_var),
    package = package,
    user = user
  )
  
  # Show message if verbose parameter is TRUE
  if (verbose) {
    # Always show message (ignore verbose setting if it doesn't exist)
    # Check verbose setting only if it explicitly exists and is FALSE
    should_print <- !(!is.null(current_config[[verbose_var]]) && !as.logical(current_config[[verbose_var]]))
    
    if (should_print) {
      # Prepare status message
      debug_status <- if (new_value) "enabled" else "disabled"
      
      # Display success message
      .icy_success(
        paste0(
          "Debug mode for ", package, " ", 
          debug_status, 
          " (", debug_var, " = ", new_value, ")",
          msg
        )
      )
    }
  }
  
  # Return invisibly for potential chaining
  return(invisible(new_value))
}
