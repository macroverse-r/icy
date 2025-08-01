#' Get the Name of the Currently Executing Package
#'
#' Intelligently determines the name of the package from which the current function is being called.
#' This utility function is essential for creating context-aware package functions that need to
#' know which package they're operating within.
#'
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
#' @param max_levels Integer. Maximum number of call stack levels to examine when searching for the calling package. Defaults to 100.
#'
#' The function uses several methods to identify the calling package:
#' 
#' 1. Checks parent environments for `.packageName`
#' 2. Examines the current namespace
#' 3. Analyzes the environment name
#' 4. Inspects the call stack for namespace qualifiers (::)
#'
#' This function is particularly useful in supporting functions that need to automatically detect
#' their package context without requiring explicit package name parameters.
#'
#' @return Character string with the package name. If it cannot be determined, this
#'   function will raise an error rather than returning NULL.
#'
#' @examples
#' \dontrun{
#' # Inside a package function, get the package name automatically
#' my_package_function <- function() {
#'   pkg_name <- get_package_name()
#'   cat("This function is running from the", pkg_name, "package\n")
#'   
#'   # Use the package name for other operations
#'   config_file <- system.file("config.yml", package = pkg_name)
#'   return(config_file)
#' }
#' 
#' # Using in a configuration function
#' get_package_config <- function() {
#'   # Automatically determine which package is calling this function
#'   pkg <- get_package_name()
#'   
#'   # Use the package name to find configuration
#'   var_names <- get_var_names(package = pkg)
#'   cat("Configuration for package", pkg, ":\n")
#'   show_config(package = pkg, var_names = var_names)
#' }
#' }
#'
#' @export
get_package_name <- function(verbose = FALSE,
                             max_levels = 100) {
  # Check if we're in the global environment
  calling_env <- parent.frame()
  if (identical(calling_env, globalenv())) {
    # Before throwing an error, check if we're in a package directory
    # This allows get_package_name() to work from R console when in a package directory
    pkg_name_from_desc <- .get_package_name_from_description()
    if (!is.null(pkg_name_from_desc) && .is_pkg_dir(package = pkg_name_from_desc)) {
      if (verbose) {
        .icy_alert(paste0("Called from global environment but in package directory: ", pkg_name_from_desc))
      }
      return(pkg_name_from_desc)
    }
    .icy_stop("`get_package_name` seems to have been called from Global Environment.")
  }
  
  current_pkg <- utils::packageName()
  pkg_name_from_desc <- .get_package_name_from_description()
  found_current_pkg <- TRUE  # We know level 0 is always current_pkg
  
  if (verbose) {
    .icy_alert(paste0("Level 0: ", current_pkg, " (skipping)"))
  }
  
  for (i in 1:max_levels) {  # Start at 1, not 0
    tryCatch({
      # Parent environments only
      pkg_name <- utils::packageName(parent.frame(n = i))
      
      # Check if we got a valid package name
      if (!is.null(pkg_name) && nzchar(pkg_name) && pkg_name != "") {
        if (pkg_name != current_pkg) {
          # Found a different package - return immediately
          if (verbose) {
            .icy_success(paste0("First non-", current_pkg, " package found at level ", i, ": ", pkg_name))
          }
          return(pkg_name)
        } else {
          # Found current package again - continue searching
          if (verbose) {
            .icy_alert(paste0("Level ", i, ": ", current_pkg, " (skipping)"))
          }
        }
      } else {
        # NULL or empty - we've reached the end of the call stack
        if (verbose) {
          .icy_alert(paste0("Level ", i, ": NULL or empty (end of call stack)"))
        }
        # Stop here - no point checking further levels
        if (is.null(pkg_name_from_desc)) {
          return(current_pkg)
        } else {
          return(pkg_name_from_desc)
        }
      }
    }, error = function(e) {
      if (verbose) {
        .icy_warn(paste0("Level ", i, ": Error - ", e$message, " (end of call stack)"))
      }
      # Stop on error too - we've gone too far
      if (is.null(pkg_name_from_desc)) {
        return(current_pkg)
      } else {
        return(pkg_name_from_desc)
      }
    })
  }
  
  # If we reach here, no non-current package was found
  if (verbose) {
    .icy_text(paste0("No non-", current_pkg, " package found, returning ", current_pkg))
  }
  return(current_pkg)
}
