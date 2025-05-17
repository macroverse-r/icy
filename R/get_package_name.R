#' Get the name of the currently executing package
#'
#' Determines the name of the package from which the current function is being called.
#' The function must be called inside a function in another package.
#'
#' @return Character string with the package name, or NULL if it cannot be determined.
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
