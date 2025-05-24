#' Load Environment Variables for Package
#'
#' Loads environment variables for a package during package startup. This function
#' is designed to be called from a package's .onLoad() function to ensure that
#' the package's environment variables are properly configured.
#'
#' The function reads the package's configuration and sets any missing environment
#' variables in the current session. It respects the priority hierarchy:
#' .Renviron values take precedence over local config values.
#'
#' @param package Character string with the package name. If NULL (default),
#'   uses the current package name.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param verbose Logical; whether to display status messages. Default is FALSE for
#'   quiet loading during package startup.
#'
#' @return Invisibly returns TRUE.
#'
#' @examples
#' \dontrun{
#' # In your package's zzz.R file:
#' .onLoad <- function(libname, pkgname) {
#'   yml2env::load(pkgname)
#' }
#'
#' # Or with custom user section:
#' .onLoad <- function(libname, pkgname) {
#'   yml2env::load(pkgname, user = "production")
#' }
#' }
#'
#' @export
load <- function(package = NULL,
                 user = "default",
                 verbose = FALSE) {
    
    # Use current package name if not provided
    if (is.null(package)) {
        package <- get_package_name()
    }
    
    # Try to get configuration with priority
    config <- tryCatch({
        get_config(
            package = package,
            user = user,
            origin = "priority"
        )
    }, error = function(e) {
        if (verbose) {
            cli::cli_alert_warning("Could not load configuration for {.pkg {package}}: {e$message}")
        }
        return(NULL)
    })
    
    if (is.null(config)) {
        return(invisible(FALSE))
    }
    
    # Set environment variables that are not already set
    vars_set <- character(0)
    for (var_name in names(config)) {
        if (Sys.getenv(var_name, unset = "") == "") {
            # Use do.call to set environment variable
            args <- list(as.character(config[[var_name]]))
            names(args) <- var_name
            do.call(Sys.setenv, args)
            vars_set <- c(vars_set, var_name)
        }
    }
    
    if (verbose && length(vars_set) > 0) {
        cli::cli_alert_success("Set {length(vars_set)} environment variable{?s} for {.pkg {package}}")
    }
    
    return(invisible(TRUE))
}