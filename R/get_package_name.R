#' Get the name of the currently executing package
#'
#' Determines the name of the package from which the current function is being called.
#'
#' @return Character string with the package name, or NULL if it cannot be determined.
#'
#' export
get_package_name <- function() {
  # First try determining if the code is part of a package
  # being loaded via library(), require(), etc.
  current_pkg <- tryCatch({
    # Get environmentName of parent env
    pkg_env <- parent.frame()
    env_name <- environmentName(topenv(pkg_env))
    
    # If the environment name starts with "package:", extract the package name
    if (startsWith(env_name, "package:")) {
      substring(env_name, 9)
    } else {
      NULL
    }
  }, error = function(e) {
    return(NULL)
  })
  
  # If packageName() didn't work, try examining the call stack
  if (is.null(current_pkg)) {
    # Get call stack
    calls <- sys.calls()
    
    # Examine the namespace of each call
    for (i in seq_along(calls)) {
      env <- parent.frame(i)
      if (exists(".__NAMESPACE__.", env)) {
        ns_env <- get(".__NAMESPACE__.", env)
        if (exists("spec", ns_env)) {
          spec <- get("spec", ns_env)
          if (!is.null(spec$name)) {
            current_pkg <- spec$name
            break
          }
        }
      }
    }
    
    # If we still don't have a package name, try one more approach
    if (is.null(current_pkg)) {
      # Try to get the package name from the environment
      for (i in rev(seq_along(calls))) {
        call_env <- parent.frame(i)
        # Try to find a package environment
        for (env in rev(search())) {
          if (startsWith(env, "package:")) {
            pkg_name <- substring(env, 9)
            # Check if this environment is the one we're looking for
            if (identical(as.environment(env), call_env)) {
              current_pkg <- pkg_name
              break
            }
          }
        }
        if (!is.null(current_pkg)) break
      }
    }
  }
  
  return(current_pkg)
}
