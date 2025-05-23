
.verbose <- function() {
  pkgname <- get_package_name()
  pkg_upper <- toupper(pkgname)
  verbose_var <- paste0(pkg_upper, "_VERBOSE")
  
  tryCatch({
    verbose <- get_env_var_values(pkgname, verbose_var)
    return(verbose)
  }, error = function(e) {
    cli::cli_alert_danger("Error in {.fun get_env_var_values}: {e$message}")
    return(TRUE)
  })
}


.debug <- function() {
  pkgname <- get_package_name()
  pkg_upper <- toupper(pkgname)
  debug_var <- paste0(pkg_upper, "_DEBUG")
  
  tryCatch({
    debug <- get_env_var_values(pkgname, debug_var)
    return(debug)
  }, error = function(e) {
    cli::cli_alert_danger("Error in {.fun get_env_var_values}: {e$message}")
    return(TRUE)
  })
}


.cur_fun <- function() {
  return(as.character(sys.call(-1)[[1]]))
}
