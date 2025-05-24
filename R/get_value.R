#' Get Single Environment Variable Value
#'
#' Retrieves the value of a single environment variable with priority resolution.
#' This function respects the priority hierarchy: .Renviron > local config > template.
#' 
#' Unlike `get_values()` which retrieves multiple values from a specific source,
#' `get_value()` retrieves a single value using the priority system.
#'
#' @param var_name Character string with the environment variable name to retrieve.
#' @param package Character string with the package name. If NULL (default),
#'   uses the current package name.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param default Optional default value to return if the variable is not found
#'   in any configuration source. If NULL (default), returns NA when not found.
#'
#' @return Character string with the variable value, or the default value if not found.
#'
#' @examples
#' \dontrun{
#' # Get a single variable value with priority resolution
#' api_key <- get_value("API_KEY", package = "mypackage")
#'
#' # Get value with a default fallback
#' db_host <- get_value("DB_HOST", package = "mypackage", default = "localhost")
#'
#' # Get value for current package
#' debug_mode <- get_value("DEBUG_MODE", default = "FALSE")
#' }
#'
#' @export
get_value <- function(var_name,
                      package = NULL,
                      user = "default",
                      default = NULL) {
    
    # Validate input
    if (length(var_name) != 1 || !is.character(var_name)) {
        cli::cli_abort("var_name must be a single character string")
    }
    
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
        if (.verbose()) {
            cli::cli_alert_warning("Could not load configuration: {e$message}")
        }
        return(list())
    })
    
    # Extract the value
    if (var_name %in% names(config)) {
        value <- config[[var_name]]
        
        # Convert to character if not already
        if (!is.character(value)) {
            value <- as.character(value)
        }
        
        return(value)
    } else {
        # Variable not found in configuration
        if (!is.null(default)) {
            return(as.character(default))
        } else {
            return(NA_character_)
        }
    }
}