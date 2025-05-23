#' Toggle or Initialize Debug Mode for Package
#'
#' @description
#' Toggles the debug mode setting for the specified package by modifying the 
#' `PKGNAME_DEBUG` variable in the package's environment variables YAML file.
#' If the variable doesn't exist, it will be created with the specified initial value.
#'
#' @param pkgname Character string. The name of the package to toggle debug mode for.
#' @param initial Logical. The initial value to use when initializing a 
#'   non-existent debug variable. Defaults to TRUE.
#' @param verbose Logical. Controls whether to display a message about the change.
#'   Defaults to TRUE. 
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
#' # Toggle debug mode for a specific package
#' toggle_debug(pkgname = "mypackage")
#'
#' # Initialize debug mode with initial FALSE
#' toggle_debug(pkgname = "otherpackage", initial = FALSE)
#'
#' @seealso
#' \code{\link{get_config_path}} for retrieving the config file path.
#'
#' @export
toggle_debug <- function(pkgname,
                         user = "default",
                         initial = TRUE,
                         verbose = TRUE) {
  
  # Create dynamic variable names based on package
  pkg_upper <- toupper(pkgname)
  debug_var <- paste0(pkg_upper, "_DEBUG")
  verbose_var <- paste0(pkg_upper, "_VERBOSE")
  
  # Get config file path
  path <- get_config_path(pkgname)
  
  # Read config
  config <- yaml::read_yaml(file = path)
  
  # Check if debug variable exists, create if it doesn't
  if (is.null(config[[user]][[debug_var]])) {
    config[[user]][[debug_var]] <- initial
    msg <- " - initialized"
  } else {
    # Toggle existing value
    config[[user]][[debug_var]] <- !config[[user]][[debug_var]]
    msg <- ""
  }
  
  # Write updated config back to file
  yaml::write_yaml(config, path)
  
  # Show message if verbose parameter is TRUE
  if (verbose) {
    # Always show message (ignore verbose setting if it doesn't exist)
    # Check verbose setting only if it explicitly exists and is FALSE
    should_print <- !(!is.null(config[[user]][[verbose_var]]) && !config[[user]][[verbose_var]])
    
    if (should_print) {
      # Prepare status message with colored output
      debug_status <- if (config[[user]][[debug_var]]) 
        cli::col_green("enabled") 
      else 
        cli::col_red("disabled")
      
      # Display success message
      cli::cli_alert_success(
        paste0(
          "Debug mode for ", pkgname, " ", 
          debug_status, 
          " (", debug_var, " = {.val ", config[[user]][[debug_var]], "})",
          msg
        )
      )
    }
  }
  
  # Return invisibly for potential chaining
  return(invisible(config[[user]][[debug_var]]))
}
