#' Sync Utility Functions for Write Operations
#'
#' @description
#' Shared helper functions for consistent sync behavior across write_local() 
#' and write_renviron() functions.
#'
#' @name sync_write_utils
NULL

#' Determine which variables to sync based on sync parameter
#'
#' @param var_names Character vector of variable names that were written
#' @param sync Sync parameter: "conservative", "all", "none", TRUE, FALSE, or character vector
#' @param original_session_vars Character vector of variables that were in session before writing
#'
#' @return Character vector of variable names to sync to session environment
#' @keywords internal
.determine_sync_vars <- function(var_names, sync, original_session_vars) {
  if (identical(sync, "none") || identical(sync, FALSE)) {
    return(character(0))
  } else if (identical(sync, "all") || identical(sync, TRUE)) {
    return(var_names)
  } else if (identical(sync, "conservative")) {
    # Only sync variables that were already in session
    return(intersect(var_names, original_session_vars))
  } else if (is.character(sync)) {
    # Explicit list of variables to sync
    return(intersect(var_names, sync))
  } else {
    .icy_stop("sync must be 'conservative', 'all', 'none', TRUE, FALSE, or character vector")
  }
}

#' Apply sync logic to session environment variables
#'
#' @param var_list Named list of variables and their new values
#' @param sync Sync parameter: "conservative", "all", "none", TRUE, FALSE, or character vector
#' @param original_session_vars Character vector of variables that were in session before writing
#' @param verbose Logical; if TRUE, shows informative messages about sync operations
#'
#' @return Character vector of variable names that were actually synced
#' @keywords internal
.apply_sync <- function(var_list, sync, original_session_vars, verbose = FALSE) {
  # Determine which variables to sync
  vars_to_sync <- .determine_sync_vars(names(var_list), sync, original_session_vars)
  
  if (length(vars_to_sync) > 0) {
    # Set session environment variables to new values
    for (var_name in vars_to_sync) {
      value <- var_list[[var_name]]
      if (is.null(value)) {
        # Remove from session environment
        if (Sys.getenv(var_name, unset = "NOT_SET") != "NOT_SET") {
          Sys.unsetenv(var_name)
        }
      } else {
        # Set in session environment
        do.call(Sys.setenv, structure(list(as.character(value)), names = var_name))
      }
    }
    
    if (verbose) {
      .icy_alert(paste0("Synced ", length(vars_to_sync), " variable", 
                            if(length(vars_to_sync) > 1) "s" else "", 
                            " to session: ", paste(vars_to_sync, collapse = ", ")))
    }
  }
  
  return(vars_to_sync)
}

#' Get current session environment variables for a package
#'
#' @param package Character string with package name
#' @param user Character string for user configuration section
#'
#' @return Character vector of variable names that are currently in session environment
#' @keywords internal
.get_current_session_vars <- function(package, user = "default") {
  # Get all possible variables from template
  template_vars <- tryCatch({
    names(get_config(package = package, origin = "template", user = user))
  }, error = function(e) {
    character(0)
  })
  
  # Check which ones are currently set in session
  session_vars <- character(0)
  for (var_name in template_vars) {
    if (Sys.getenv(var_name, unset = "NOT_SET") != "NOT_SET") {
      session_vars <- c(session_vars, var_name)
    }
  }
  
  return(session_vars)
}