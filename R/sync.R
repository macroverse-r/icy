#' Sync Environment Variables with Configuration
#'
#' Updates the current R session's environment variables to match the configuration
#' values, respecting the priority hierarchy (.Renviron > local config). This function
#' is useful after modifying configuration files to apply changes without restarting R.
#'
#' `sync()` will update existing variables to match the configuration values.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param var_names Optional character vector of specific variable names to sync.
#'   If NULL (default), syncs all variables defined in the configuration.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
#'
#' @return Invisibly returns TRUE.
#'
#' @examples
#' \dontrun{
#' # After modifying local config, sync all variables
#' sync("mypackage")
#'
#' # Sync specific variables only
#' sync("mypackage", var_names = c("API_KEY", "DB_HOST"))
#'
#' # Sync quietly
#' sync("mypackage", verbose = FALSE)
#' }
#'
#' @export
sync <- function(package = get_package_name(),
                 var_names = NULL,
                 section = "default",
                 verbose = FALSE) {
    
    # Get configuration from files only (not session) to sync session to match files
    # Priority order: .Renviron > local config (but exclude session environment)
    renviron_config <- tryCatch({
        get_config(package = package, origin = "renviron", section = section)
    }, error = function(e) list())
    
    local_config <- tryCatch({
        get_config(package = package, origin = "local", section = section)
    }, error = function(e) list())
    
    # Merge with .Renviron taking priority over local config
    config <- local_config
    for (var_name in names(renviron_config)) {
        config[[var_name]] <- renviron_config[[var_name]]
    }
    
    # If specific var_names provided, filter config
    if (!is.null(var_names)) {
        config <- config[intersect(names(config), var_names)]
        
        # Check for missing variables
        missing_vars <- setdiff(var_names, names(config))
        if (length(missing_vars) > 0 && verbose) {
            .icy_warn(paste0("Variables not found in configuration: ", paste(missing_vars, collapse = ", ")))
        }
    }
    
    if (length(config) == 0) {
        if (verbose) {
            .icy_alert("No variables to sync")
        }
        return(invisible(TRUE))
    }
    
    # Update environment variables
    updated_vars <- character(0)
    for (var_name in names(config)) {
        old_value <- Sys.getenv(var_name, unset = NA)
        new_value <- config[[var_name]]
        
        # Handle NULL values - skip setting environment variables for NULL config values
        if (is.null(new_value)) {
            next
        }
        
        new_value <- as.character(new_value)
        
        if (verbose) {
            .icy_text(paste0("Checking ", var_name, ": session='", 
                           ifelse(is.na(old_value), "(not set)", old_value), 
                           "', config='", new_value, "'"))
        }
        
        if (is.na(old_value) || old_value != new_value) {
            # Use do.call to set environment variable
            args <- list(new_value)
            names(args) <- var_name
            do.call(Sys.setenv, args)
            updated_vars <- c(updated_vars, var_name)
            
            if (verbose) {
                .icy_text(paste0("Updated ", var_name, " from '", 
                               ifelse(is.na(old_value), "(not set)", old_value), 
                               "' to '", new_value, "'"))
            }
        }
    }
    
    if (verbose) {
        if (length(updated_vars) > 0) {
            .icy_success(paste0("Updated ", length(updated_vars), " environment variable", if(length(updated_vars) > 1) "s" else ""))
            
            # Show current values
            show_config(package = package, var_names = names(config))
        } else {
            .icy_alert("All variables already up to date")
        }
    }
    
    return(invisible(TRUE))
}
