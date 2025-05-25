#' Sync Environment Variables with Configuration
#'
#' Updates the current R session's environment variables to match the configuration
#' values, respecting the priority hierarchy (.Renviron > local config). This function
#' is useful after modifying configuration files to apply changes without restarting R.
#'
#' Unlike `load()` which only sets missing variables, `sync()` will update existing
#' variables to match the configuration values.
#'
#' @param package Character string with the package name. If NULL (default),
#'   uses the current package name.
#' @param var_names Optional character vector of specific variable names to sync.
#'   If NULL (default), syncs all variables defined in the configuration.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param verbose Logical; whether to display status messages. Default is TRUE.
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
sync <- function(package = NULL,
                 var_names = NULL,
                 user = "default",
                 verbose = TRUE) {
    
    # Use current package name if not provided
    if (is.null(package)) {
        package <- get_package_name()
    }
    
    # Get configuration with priority
    config <- tryCatch({
        get_config(
            package = package,
            user = user,
            origin = "priority"
        )
    }, error = function(e) {
        cli::cli_abort("Could not load configuration for {.pkg {package}}: {e$message}")
    })
    
    # If specific var_names provided, filter config
    if (!is.null(var_names)) {
        config <- config[intersect(names(config), var_names)]
        
        # Check for missing variables
        missing_vars <- setdiff(var_names, names(config))
        if (length(missing_vars) > 0 && verbose) {
            cli::cli_alert_warning("Variables not found in configuration: {.var {missing_vars}}")
        }
    }
    
    if (length(config) == 0) {
        if (verbose) {
            cli::cli_alert_info("No variables to sync")
        }
        return(invisible(TRUE))
    }
    
    # Update environment variables
    updated_vars <- character(0)
    for (var_name in names(config)) {
        old_value <- Sys.getenv(var_name, unset = NA)
        new_value <- as.character(config[[var_name]])
        
        if (is.na(old_value) || old_value != new_value) {
            # Use do.call to set environment variable
            args <- list(new_value)
            names(args) <- var_name
            do.call(Sys.setenv, args)
            updated_vars <- c(updated_vars, var_name)
        }
    }
    
    if (verbose) {
        if (length(updated_vars) > 0) {
            cli::cli_alert_success("Updated {length(updated_vars)} environment variable{?s}")
            
            # Show current values
            show_config(package = package, var_names = names(config))
        } else {
            cli::cli_alert_info("All variables already up to date")
        }
    }
    
    return(invisible(TRUE))
}
