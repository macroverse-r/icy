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
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param type Character string specifying the type of configuration files to locate.
#'   Options: "local" (user-specific configs, default) or "template" (package-provided configs).
#' @param pkg_first Logical. If TRUE, checks if the current working directory is the package directory first. Defaults to TRUE.
#' @param user_dir Deprecated. Use `type` parameter instead. Logical for backwards compatibility.
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
                            type = "local",
                            pkg_first = TRUE,
                            user_dir = NULL,
                            ud_which = "config") {

  # Handle deprecated user_dir parameter
  if (!is.null(user_dir)) {
    .icy_warn("Parameter 'user_dir' is deprecated. Use 'type' parameter instead.")
    type <- if (user_dir) "local" else "template"
  }
  
  # Validate type parameter
  if (!type %in% c("local", "template")) {
    .icy_stop("Parameter 'type' must be either 'local' or 'template'")
  }
  
  # First try: use current working directory if it seems to be a R package directory
  if (pkg_first && .is_pkg_dir(package = package)) {
    if (type == "template") {
      return(file.path(getwd(), "inst"))
    } else {  # type == "local"
      return(file.path(getwd(), "inst", "local_config"))
    }
  }
  
  # Second try: for installed package
  if (type == "template") {
    # Templates are in package installation directory
    path <- suppressWarnings(system.file(package = package))
  } else {  # type == "local"
    # Local configs are in user config directory
    path <- suppressWarnings(tools::R_user_dir(package = package, which = ud_which))
  }

  if (path != "") {
    # Remove any trailing "/"
    path <- clean_dir_path(path, check_exists = FALSE)
    # If path ends with "/inst", strip it off
    if (endsWith(path, "/inst")) {
      path <- sub("/inst$", "", path)
    }
  }

  return(path)
}
