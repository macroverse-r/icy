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
#'
#' @return Character string with the absolute path to the package's root directory.
#'
#' @examples
#' \dontrun{
#' # Get the package directory
#' pkg_dir <- get_package_dir("mypackage")
#' 
#' # Use it to locate resources in the package
#' config_path <- file.path(pkg_dir, "inst", "config", "default.yml")
#' data_dir <- file.path(pkg_dir, "data")
#' 
#' # Use in a function that needs to find files in the package directory
#' get_package_resources <- function(pkg_name) {
#'   base_dir <- get_package_dir(pkg_name)
#'   templates_dir <- file.path(base_dir, "inst", "templates")
#'   available_templates <- list.files(templates_dir, pattern = "\\.Rmd$")
#'   return(available_templates)
#' }
#' }
#'
#' @export
get_package_dir <- function(package, user_dir = TRUE, pkg_loc_first = TRUE) {
  # First try: use current working directory if it seems to be a R package directory
  if (pkg_loc_first) {
    current_dir <- getwd()
    desc_file <- file.path(current_dir, "DESCRIPTION")
    r_dir <- file.path(current_dir, "R")

    cli::cli_inform("basename(current_dir) = {basename(current_dir)}")
    cli::cli_inform("desc_file = {desc_file}")
    cli::cli_inform("r_dir = {r_dir}")

    if (basename(current_dir) == package &&
          file.exists(desc_file) &&
          dir.exists(r_dir)) {
      return(current_dir)
    }
  }
  
  # Second try: for installed package
  # use tools::R_user_dir() if user_dir is TRUE (default)
  if (user_dir) {
    path <- suppressWarnings(tools::R_user_dir(package = package, "config"))
  } else {
    # use system.file if not referring to user_dir
    path <- suppressWarnings(system.file(package = package))
  }

  if (path != "") {
    # Remove any trailing "/"
    path <- sub("/$", "", path)
    # If path ends with "/inst", strip it off
    if (endsWith(path, "/inst")) {
      path <- sub("/inst$", "", path)
    }
    return(path)
  }
  
  # Fallback to current directory
  return(current_dir)
}
