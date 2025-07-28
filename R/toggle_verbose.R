#' Toggle or Initialize Verbose Mode for Package
#'
#' @description
#' Toggles the verbose mode setting for the specified package by modifying the
#' `PKGNAME_VERBOSE` variable in the package's environment variables YAML file.
#' If the variable doesn't exist, it will be created with the specified initial value.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param user Character string. The user configuration to modify. Defaults to "default".
#' @param initial Logical. The default value to use when initializing a
#'   non-existent verbose variable. Defaults to TRUE.
#' @param verbose Logical. If TRUE, displays informative messages about the operation. If NULL (default), uses the package's VERBOSE configuration value, or FALSE if not set.
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
#' \dontrun{
#' # Toggle verbose mode for a specific package
#' toggle_verbose(package = "mypackage")
#'
#' # Initialize verbose mode with initial FALSE
#' toggle_verbose(package = "mypackage", initial = FALSE)
#'
#' # Toggle without displaying a message
#' toggle_verbose(package = "mypackage", verbose = FALSE)
#' }
#'
#' @seealso
#' \code{\link{toggle_debug}} for toggling debug mode.
#' \code{\link{get_config}} for retrieving the config data.
#'
#' @export
toggle_verbose <- function(package = get_package_name(),
                           user = "default",
                           initial = TRUE,
                           verbose = NULL) {
  # Create dynamic variable names based on package
  pkg_upper <- toupper(package)
  verbose_var <- paste0(pkg_upper, "_VERBOSE")

  # Read current config
  current_config <- tryCatch(
    {
      get_config(package = package, origin = "local", user = user)
    },
    error = function(e) {
      list()
    }
  )
  
  # Set verbose default from config if not explicitly provided
  if (is.null(verbose)) verbose <- TRUE

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
    var_list = structure(list(new_value), names = verbose_var),
    package = package,
    user = user
  )

  # Show message if verbose parameter is TRUE
  if (verbose) {
    # Prepare status message with colored output
    verbose_status <- if (new_value) {
      .apply_color("enabled", "green")
    } else {
      .apply_color("disabled", "red")
    }

    # Display success message
    .icy_alert_success(
      paste0(
        "Verbose mode for ", package, " ",
        verbose_status,
        " (", verbose_var, " = ", new_value, ")",
        msg
      )
    )
  }

  # Return invisibly for potential chaining
  return(invisible(new_value))
}
