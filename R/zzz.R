#' Package startup
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  create_config(package = "icy", verbose = FALSE, overwrite = FALSE)
}
