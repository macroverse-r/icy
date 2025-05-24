#' Toggle or Initialize Verbose Mode for Package
#'
#' @description
#' Toggles the verbose mode setting for the specified package by modifying the 
#' `PKGNAME_VERBOSE` variable in the package's environment variables YAML file.
#' If the variable doesn't exist, it will be created with the specified initial value.
#'
#' @param pkgname Character string. The name of the package to toggle verbose mode for.
#' @param initial Logical. The default value to use when initializing a 
#'   non-existent verbose variable. Defaults to TRUE.
#' @param verbose Logical. Controls whether to display a message about the change.
#'   Defaults to TRUE.
#'
#' @return Invisibly returns the new value of the verbose variable.
#'
#' @details
#' The function uses the provided package name (or determines it dynamically) 
#' and creates the appropriate verbose variable name (`PKGNAME_VERBOSE` where PKGNAME
#' is the uppercase package name). It reads the configuration from the local YAML file,
#' toggles or initializes the verbose variable, and writes the updated configuration
#' back to the file.
#'
#' Unlike the debug toggle function, this function will display a message about the change
#' only if the `verbose` parameter is TRUE, regardless of the current verbose setting
#' in the configuration.
#'
#' @examples
#' # Toggle verbose mode for a specific package
#' toggle_verbose(pkgname = "mypackage")
#'
#' # Initialize verbose mode with initial FALSE
#' toggle_verbose(pkgname = "mypackage", initial = FALSE)
#'
#' # Toggle without displaying a message
#' toggle_verbose(pkgname = "mypackage", verbose = FALSE)
#'
#' @seealso
#' \code{\link{toggle_debug}} for toggling debug mode.
#' \code{\link{get_config_path}} for retrieving the config file path.
#'
#' @export
toggle_verbose <- function(pkgname,
                           user = "default",
                           initial = TRUE,
                           verbose = TRUE) {
  
  # Create dynamic variable names based on package
  pkg_upper <- toupper(pkgname)
  verbose_var <- paste0(pkg_upper, "_VERBOSE")
  
  # Read current config
  current_config <- tryCatch({
    get_config(package = pkgname, user = user, origin = "local")
  }, error = function(e) {
    list()
  })
  
  # Check if verbose variable exists
  if (is.null(current_config[[verbose_var]])) {
    new_value <- initial
    msg <- " - initialized"
  } else {
    # Toggle existing value
    current_value <- as.logical(current_config[[verbose_var]])
    new_value <- !current_value
    msg <- ""
  }
  
  # Write updated value to local config
  write_local(
    var_list = setNames(list(new_value), verbose_var),
    package = pkgname,
    user = user
  )
  
  # Show message if verbose parameter is TRUE
  if (verbose) {
    # Prepare status message with colored output
    verbose_status <- if (new_value) 
      cli::col_green("enabled") 
    else 
      cli::col_red("disabled")
    
    # Display success message
    cli::cli_alert_success(
      paste0(
        "Verbose mode for ", pkgname, " ", 
        verbose_status, 
        " (", verbose_var, " = {.val ", new_value, "})",
        msg
      )
    )
  }
  
  # Return invisibly for potential chaining
  return(invisible(new_value))
}
