
.verbose <- function() {
  # icy must read its own settings directly from environment variables
  # to avoid circular dependency
  verbose_val <- Sys.getenv("ICY_VERBOSE", "FALSE")
  return(as.logical(verbose_val))
}


.debug <- function() {
  # icy must read its own settings directly from environment variables
  # to avoid circular dependency
  debug_var <- paste0(toupper("icy"), "_DEBUG")
  debug_val <- Sys.getenv(debug_var, "FALSE")
  return(as.logical(debug_val))
}


cur_fun <- function() {
  print(sys.call())
  return(as.character(sys.call(-1)[[2]]))
}

#' Check if a value is NULL and provide a default
#'
#' @param x The value to check
#' @param default The default value to use if x is NULL
#'
#' @return x if not NULL, otherwise default
#' @keywords internal
`%||%` <- function(x, default) {
    if (is.null(x)) default else x
}

