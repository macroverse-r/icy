#' Get Configuration Directory (Internal)
#'
#' Returns the directory path where configuration files are stored for a package.
#'
#' @param package Character string with the package name.
#' @param type Character string: "local" (user config dir) or "template" (package template dir).
#'
#' @return Character string with the absolute path to the configuration directory.
#' @keywords internal
.get_config_dir <- function(package, type = "local") {
  if (!type %in% c("local", "template")) {
    .icy_stop("Parameter 'type' must be either 'local' or 'template'")
  }

  if (type == "local") {
    # Single source of truth: always user config directory
    return(tools::R_user_dir(package = package, which = "config"))
  }

  # type == "template": needs dev/installed branching
  if (.is_pkg_dir(package = package)) {
    return(file.path(getwd(), "inst"))
  }

  # Installed package: system.file() returns the package root where templates live
  path <- suppressWarnings(system.file(package = package))
  if (path != "") {
    path <- clean_dir_path(path, check_exists = FALSE)
  }

  return(path)
}
