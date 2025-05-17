#' Display Environment Variable Values
#'
#' Displays the current values of specified environment variables in the R session.
#' This function provides a user-friendly output of variable values with formatting
#' appropriate for different types of variables (e.g., directories vs. other values).
#'
#' This function is useful for:
#' 
#' * Quickly checking current environment variable configurations
#' * Verifying that environment variables were set correctly
#' * Debugging environment-dependent behavior in packages
#' * Showing users their current configuration
#'
#' @param var_names Character vector of variable names to display.
#'   If NULL (default), attempts to get all variables from the package's YAML config.
#'   
#' @return Invisibly returns a named list of the displayed variables and their values.
#'   This makes it useful for both display and programmatic inspection.
#'
#' @examples
#' \dontrun{
#' # Display all environment variables defined for a package
#' display_env_vars(get_env_var_names(package = "mypackage"))
#' 
#' # Display specific variables
#' display_env_vars(c("MY_PACKAGE_DATA_DIR", "MY_PACKAGE_API_KEY"))
#' 
#' # Store values for programmatic use while displaying them
#' values <- display_env_vars(c("MY_PACKAGE_API_URL", "MY_PACKAGE_DEBUG"))
#' if (values$MY_PACKAGE_DEBUG == "TRUE") {
#'   # Enable additional debugging features
#' }
#' 
#' # Using in a package status function
#' show_package_config <- function() {
#'   pkg <- "mypackage"
#'   var_names <- get_env_var_names(package = pkg)
#'   cat("Current package configuration:\n")
#'   display_env_vars(var_names)
#' }
#' }
#' 
#' @export
display_env_vars <- function(var_names = NULL) {
  if (is.null(var_names)) {
    var_names <- get_env_var_names()
  }
  
  if (length(var_names) == 0) {
    return(invisible(list()))
  }
  
  # Create a list to store values
  values_list <- list()
  
  cli::cli_h3("Current environment variable values:")
  for (var in var_names) {
    value <- Sys.getenv(var, "(not set)")
    values_list[[var]] <- value
    
    # Format paths with .file instead of .val for consistency
    if (grepl("_DIR$", var) && value != "(not set)") {
      cli::cli_text("{.var {var}} = {.file {value}}")
    } else {
      cli::cli_text("{.var {var}} = {.val {value}}")
    }
  }
  
  invisible(values_list)
}
