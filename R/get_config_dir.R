#' Get Configuration Directory
#' 
#' Returns the directory path where configuration files are stored for a package.
#' This function handles both development and installed package scenarios.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` 
#'   to detect the calling package.
#' @param type Character string specifying the type of configuration files to locate.
#'   Options: "local" (user-specific configs, default) or "template" (package-provided configs).
#'
#' @return Character string with the absolute path to the configuration directory.
#'
#' @details
#' For `type = "local"`:
#' \itemize{
#'   \item In development: `{package_root}/inst/local_config/`
#'   \item When installed: `{user_config_dir}/{package}/`
#' }
#' 
#' For `type = "template"`:
#' \itemize{
#'   \item In development: `{package_root}/inst/`
#'   \item When installed: `{package_install_dir}/`
#' }
#'
#' @examples
#' \dontrun{
#' # Get local config directory
#' local_dir <- get_config_dir("mypackage", type = "local")
#' 
#' # Get template directory
#' template_dir <- get_config_dir("mypackage", type = "template")
#' }
#'
#' @export
get_config_dir <- function(package = get_package_name(), type = "local") {
  # Validate type parameter
  if (!type %in% c("local", "template")) {
    .icy_stop("Parameter 'type' must be either 'local' or 'template'")
  }
  
  # Try development scenario first
  if (.is_pkg_dir(package = package)) {
    if (type == "template") {
      return(file.path(getwd(), "inst"))
    } else {  # type == "local"
      return(file.path(getwd(), "inst", "local_config"))
    }
  }
  
  # Installed package scenario
  if (type == "template") {
    # Templates are in package installation directory
    path <- suppressWarnings(system.file(package = package))
  } else {  # type == "local"
    # Local configs are in user config directory
    path <- suppressWarnings(tools::R_user_dir(package = package, which = "config"))
  }

  if (path != "") {
    # Remove any trailing "/"
    path <- clean_dir_path(path, check_exists = FALSE)
    # If path ends with "/inst", strip it off for template type
    if (type == "template" && endsWith(path, "/inst")) {
      path <- sub("/inst$", "", path)
    }
  }

  return(path)
}
