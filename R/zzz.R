#' Package startup
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  create_local(package = "icy")
}
