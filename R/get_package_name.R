#' Get the Name of the Currently Executing Package
#'
#' Intelligently determines the name of the package from which the current function is being called.
#' This utility function is essential for creating context-aware package functions that need to
#' know which package they're operating within.
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
get_package_name <- function() {

  # Method 1: Check all parent environments for .packageName
  env <- parent.frame()
  while (!identical(env, emptyenv())) {
    if (exists(".packageName", envir = env, inherits = FALSE)) {
      pkg_name <- get(".packageName", envir = env)
      if (pkg_name != "") return(pkg_name)
    }
    env <- parent.env(env)
  }

  # Method 2: If in a namespace, get the namespace name
  env <- topenv(parent.frame())
  if (isNamespace(env)) {
    return(getNamespaceName(env))
  }

  # Method 3: Get environment name
  env <- topenv(parent.frame())
  env_name <- environmentName(env)
  if (env_name != "" && env_name != "R_GlobalEnv") {
    return(gsub("^package:", "", env_name))
  }

  calls <- sys.calls()
  for (i in seq_along(calls)) {
    call <- calls[[i]]
    if (is.call(call) && length(call) >= 3 && identical(call[[1]], as.name("::"))) {
      return(as.character(call[[2]]))
    }
  }
  
  # Check if we're in the global environment
  if (identical(env, globalenv())) {
    cli::cli_abort("`get_package_name` seems to be called from Global Environment.")
  } else {
    cli::cli_abort("`get_package_name` could not find the package from which it is called.")

  }
  
  return(invisible(NULL))
}
