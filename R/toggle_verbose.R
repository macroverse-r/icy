#' Toggle or Initialize Verbose Mode for Package
#'
#' @description
#' Toggles the verbose mode setting for the specified package by modifying the
#' `PKGNAME_VERBOSE` variable in the package's environment variables YAML file.
#' If the variable doesn't exist, it will be created with the specified initial value.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param section Character string. The section configuration to modify. Defaults to "default".
#' @param initial Logical. The default value to use when initializing a
#'   non-existent verbose variable. Defaults to TRUE.
#' @param verbose Logical. If TRUE, displays informative messages about the operation. If NULL (default), defaults to TRUE.
#'
#' @return Invisibly returns the new value of the verbose variable.
#'
#' @details
#' The function uses the provided package name (or determines it dynamically)
#' and creates the appropriate verbose variable name (`PKGNAME_VERBOSE` where PKGNAME
#' is the uppercase package name). It reads the configuration from the YAML config file,
#' toggles or initializes the verbose variable, and writes the updated configuration
#' back to the file.
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
                           section = "default",
                           initial = TRUE,
                           verbose = NULL) {
  pkg_upper <- toupper(package)
  verbose_var <- paste0(pkg_upper, "_VERBOSE")

  current_config <- tryCatch({
    get_config(package = package, section = section)
  }, error = function(e) list())

  if (is.null(verbose)) verbose <- TRUE

  if (is.null(current_config[[verbose_var]])) {
    new_value <- initial
    msg <- " - initialized"
  } else {
    new_value <- !as.logical(current_config[[verbose_var]])
    msg <- ""
  }

  update_config(
    var_list = structure(list(new_value), names = verbose_var),
    package = package,
    section = section
  )

  if (verbose) {
    status <- if (new_value) "enabled" else "disabled"
    .icy_success(paste0(
      "Verbose mode for ", package, " ", status,
      " (", verbose_var, " = ", new_value, ")", msg
    ))
  }

  return(invisible(new_value))
}
