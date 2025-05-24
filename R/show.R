#' Show Environment Variable Status
#'
#' Displays the current values and status of environment variables, including
#' information about where each value comes from (e.g., .Renviron, local config,
#' or not set). This function provides comprehensive visibility into the
#' configuration state.
#'
#' @param package Character string with the package name. If NULL (default),
#'   uses the current package name.
#' @param var_names Optional character vector of specific variable names to show.
#'   If NULL (default), shows all variables defined in the configuration.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param show_source Logical; whether to show the source of each value
#'   (e.g., ".Renviron", "local config", "not set"). Default is TRUE.
#'
#' @return Invisibly returns a data frame with variable names, values, and sources.
#'
#' @examples
#' \dontrun{
#' # Show all environment variables for a package
#' show("mypackage")
#'
#' # Show specific variables only
#' show("mypackage", var_names = c("API_KEY", "DB_HOST"))
#'
#' # Show without source information
#' show("mypackage", show_source = FALSE)
#'
#' # Capture the results for programmatic use
#' status <- show("mypackage")
#' unset_vars <- status$variable[status$source == "not set"]
#' }
#'
#' @export
show <- function(package = NULL,
                 var_names = NULL,
                 user = "default",
                 show_source = TRUE) {
    
    # Use current package name if not provided
    if (is.null(package)) {
        package <- get_package_name()
    }
    
    # Get all possible variable names if not specified
    if (is.null(var_names)) {
        # Try to get from template to see all possible variables
        template_vars <- tryCatch({
            names(get_config(package = package, user = user, origin = "template"))
        }, error = function(e) NULL)
        
        # Also get from local config
        local_vars <- tryCatch({
            names(get_config(package = package, user = user, origin = "local"))
        }, error = function(e) NULL)
        
        var_names <- unique(c(template_vars, local_vars))
        
        if (length(var_names) == 0) {
            cli::cli_alert_warning("No variables found in configuration for {.pkg {package}}")
            return(invisible(data.frame(variable = character(), value = character(), source = character())))
        }
    }
    
    # Build status information
    status_df <- data.frame(
        variable = var_names,
        value = character(length(var_names)),
        source = character(length(var_names)),
        stringsAsFactors = FALSE
    )
    
    # Get values from different sources
    renviron_config <- tryCatch({
        get_config(package = package, user = user, origin = "renviron")
    }, error = function(e) list())
    
    local_config <- tryCatch({
        get_config(package = package, user = user, origin = "local")
    }, error = function(e) list())
    
    # Check each variable
    for (i in seq_along(var_names)) {
        var <- var_names[i]
        
        # Check current session first
        session_value <- Sys.getenv(var, unset = NA)
        
        if (!is.na(session_value)) {
            status_df$value[i] <- session_value
            
            # Determine source
            if (var %in% names(renviron_config) && renviron_config[[var]] == session_value) {
                status_df$source[i] <- ".Renviron"
            } else if (var %in% names(local_config) && local_config[[var]] == session_value) {
                status_df$source[i] <- "local config"
            } else {
                status_df$source[i] <- "session"
            }
        } else {
            status_df$value[i] <- "(not set)"
            status_df$source[i] <- "not set"
        }
    }
    
    # Display results
    cli::cli_h3("Environment variables for {.pkg {package}}:")
    
    for (i in seq_len(nrow(status_df))) {
        var <- status_df$variable[i]
        value <- status_df$value[i]
        source <- status_df$source[i]
        
        # Format display based on variable type and source
        if (value == "(not set)") {
            if (show_source) {
                cli::cli_text("{.var {var}} = {.val {value}}")
            } else {
                cli::cli_text("{.var {var}} = {.val {value}}")
            }
        } else {
            # Format paths with .file
            if (grepl("_DIR$|_PATH$", var)) {
                if (show_source) {
                    cli::cli_text("{.var {var}} = {.file {value}} [{.emph {source}}]")
                } else {
                    cli::cli_text("{.var {var}} = {.file {value}}")
                }
            } else {
                if (show_source) {
                    cli::cli_text("{.var {var}} = {.val {value}} [{.emph {source}}]")
                } else {
                    cli::cli_text("{.var {var}} = {.val {value}}")
                }
            }
        }
    }
    
    return(invisible(status_df))
}