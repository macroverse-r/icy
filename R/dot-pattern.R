
#' Swap Config/Template Filename
#'
#' Converts a config filename to its template counterpart or vice versa.
#'
#' @param filename Character string with a config or template filename.
#' @return Character string with the swapped filename.
#' @keywords internal
.swap_filename <- function(filename) {
  base <- tools::file_path_sans_ext(filename)
  ext <- tools::file_ext(filename)
  if (ext == "") ext <- "yml"

  if (grepl("_config$", base)) {
    paste0(sub("_config$", "_template", base), ".", ext)
  } else if (grepl("_template$", base)) {
    paste0(sub("_template$", "_config", base), ".", ext)
  } else {
    filename
  }
}

#' Generate Config Filename
#'
#' Generates the deterministic config filename for a package.
#'
#' @param package Character string with the package name.
#' @param name Optional character string for named configs (e.g., "gams_switches").
#' @return Character string with the config filename (e.g., "mypackage_config.yml").
#' @keywords internal
.config_filename <- function(package, name = NULL) {
  if (is.null(name)) paste0(package, "_config.yml")
  else paste0(package, "_", name, "_config.yml")
}

#' Generate Template Filename
#'
#' Generates the deterministic template filename for a package.
#'
#' @param package Character string with the package name.
#' @param name Optional character string for named configs (e.g., "gams_switches").
#' @return Character string with the template filename (e.g., "mypackage_template.yml").
#' @keywords internal
.template_filename <- function(package, name = NULL) {
  if (is.null(name)) paste0(package, "_template.yml")
  else paste0(package, "_", name, "_template.yml")
}
