
.verbose <- function() {
  # yml2env must read its own settings directly from environment variables
  # to avoid circular dependency
  verbose_val <- Sys.getenv("YML2ENV_VERBOSE", "FALSE")
  return(as.logical(verbose_val))
}


.debug <- function() {
  # yml2env must read its own settings directly from environment variables
  # to avoid circular dependency
  debug_val <- Sys.getenv("YML2ENV_DEBUG", "FALSE")
  return(as.logical(debug_val))
}


cur_fun <- function() {
  print(sys.call())
  return(as.character(sys.call(-1)[[2]]))
}
