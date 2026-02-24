#' Toggle or Initialize Debug Mode for Package
#'
#' @description
#' Toggles the debug mode setting for the specified package by modifying the
#' `PKGNAME_DEBUG` variable in the package's environment variables YAML file.
#' If the variable doesn't exist, it will be created with the specified initial value.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param section Character string. The section configuration to modify. Defaults to "default".
#' @param initial Logical. The initial value to use when initializing a
#'   non-existent debug variable. Defaults to TRUE.
#' @param verbose Logical. If TRUE, displays informative messages about the operation. If NULL (default), defaults to TRUE.
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
#' \code{\link{toggle_verbose}} for toggling verbose mode.
#' \code{\link{get_config}} for retrieving the config data.
#'
#' @export
toggle_debug <- function(package = get_package_name(),
                         section = "default",
                         initial = TRUE,
                         verbose = NULL) {
  pkg_upper <- toupper(package)
  debug_var <- paste0(pkg_upper, "_DEBUG")

  current_config <- tryCatch({
    get_config(package = package, section = section)
  }, error = function(e) list())

  if (is.null(verbose)) verbose <- TRUE

  if (is.null(current_config[[debug_var]])) {
    new_value <- initial
    msg <- " - initialized"
  } else {
    new_value <- !as.logical(current_config[[debug_var]])
    msg <- ""
  }

  write_local(
    var_list = structure(list(new_value), names = debug_var),
    package = package,
    section = section
  )

  if (verbose) {
    status <- if (new_value) "enabled" else "disabled"
    .icy_success(paste0(
      "Debug mode for ", package, " ", status,
      " (", debug_var, " = ", new_value, ")", msg
    ))
  }

  return(invisible(new_value))
}
