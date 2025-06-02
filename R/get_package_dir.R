#' Get the Root Directory of a Package
#' 
#' Intelligently determines the root directory of a package, handling both development
#' (git clone) and installed package scenarios. This is particularly useful for functions
#' that need to reference files relative to the package root regardless of how the package 
#' is being used.
#'
#' The function uses a multi-step strategy:
#' 
#' 1. Checks if the current working directory appears to be the package directory
#' 2. Uses `tools::R_user_dir()` or `system.file()` to locate an installed package
#' 3. Falls back to the current directory if all else fails
#'
#' This function is especially helpful for accessing package resources in a way that works
#' consistently for both package developers and end users.
#'
#' @param package Character string with the package name to locate.
#' @param user_dir Logical. If TRUE, uses `tools::R_user_dir()` to locate the package directory. Defaults to TRUE.
#' @param pkg_first Logical. If TRUE, checks if the current working directory is the package directory first. Defaults to TRUE.
#' @param ud_which Character string. Passed to `tools::R_user_dir()` which parameter. Defaults to "config".
#'
#' @return Character string with the absolute path to the package's root directory.
#'
#' @examples
#' \dontrun{
#' # Get the package directory
#' pkg_dir <- get_package_path("mypackage")
#' 
#' # Use it to locate resources in the package
#' config_path <- file.path(pkg_dir, "inst", "config", "default.yml")
#' data_dir <- file.path(pkg_dir, "data")
#' 
#' # Use in a function that needs to find files in the package directory
#' get_package_resources <- function(pkg_name) {
#'   base_dir <- get_package_path(pkg_name)
#'   templates_dir <- file.path(base_dir, "inst", "templates")
#'   available_templates <- list.files(templates_dir, pattern = "\\.Rmd$")
#'   return(available_templates)
#' }
#' }
#'
#' @export
get_package_path <- function(package = get_package_name(),
                            user_dir = TRUE,
                            pkg_first = TRUE,
                            ud_which = "config") {

  # First try: use current working directory if it seems to be a R package directory
  if (pkg_first && .is_pkg_dir(package = package)) {
    return(getwd())
  }
  
  # Second try: for installed package
  # use tools::R_user_dir() if user_dir is TRUE (default)
  if (user_dir) {
    path <- suppressWarnings(tools::R_user_dir(package = package,
                                               which = ud_which))
  } else {
    # use system.file if not referring to user_dir
    path <- suppressWarnings(system.file(package = package))
  }

  if (path != "") {
    # Remove any trailing "/"
    path <- .clean_dir_path(path)
    # If path ends with "/inst", strip it off
    if (endsWith(path, "/inst")) {
      path <- sub("/inst$", "", path)
    }
  }

  return(path)
}
