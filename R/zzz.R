#' Package startup
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  create_local(package = "icy", verbose = FALSE, overwrite = FALSE)
}

# Helper for NULL coalescing 
`%||%` <- function(x, y) if (is.null(x)) y else x
