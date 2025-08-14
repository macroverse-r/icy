
#' Check if a value is NULL and provide a default
#'
#' @name grapes-or-or-grapes
#' @param x The value to check
#' @param default The default value to use if x is NULL
#'
#' @return x if not NULL, otherwise default
#' @keywords internal
`%||%` <- function(x, default) {
    if (is.null(x)) default else x
}

