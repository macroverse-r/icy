#' Sync Environment Variables from .Renviron to Current Session
#'
#' Makes the environment variables in the current R session match exactly what's in the 
#' .Renviron file. This function ensures consistency between the persistent environment 
#' configuration and the active R session.
#'
#' This function is essential when making changes to the .Renviron file during an R session,
#' as changes to this file are normally only loaded when R starts. It's typically called
#' after using `write_to_renviron()` to make the newly written variables immediately 
#' available without restarting R.
#'
#' @param var_names Character vector of environment variable names to sync. These should
#'   be the names of the variables that you want to be updated from the .Renviron file.
#' @param verbose Logical; whether to display status messages and current values.
#'   Default is TRUE.
#' 
#' @return Invisibly returns NULL.
#'
#' @examples
#' \dontrun{
#' # After writing variables to .Renviron, sync them to the current session
#' write_to_renviron(
#'   var_list = list(MY_PACKAGE_DIR = "/path/to/data"),
#'   package = "mypackage"
#' )
#' sync_env_vars("MY_PACKAGE_DIR")
#' 
#' # Get environment variable list from a package's YAML config and sync all
#' pkg_vars <- get_env_var_names(package = "mypackage")
#' sync_env_vars(pkg_vars)
#' 
#' # Sync without printing status messages
#' sync_env_vars(c("MY_PACKAGE_DIR", "MY_PACKAGE_CONFIG"), verbose = FALSE)
#' }
#' 
#' @export
sync_env_vars <- function(var_names, verbose = TRUE) {
  
  # Get path to .Renviron file
  renviron_path <- .get_renviron_path()
  # Check file exists
  if (!file.exists(renviron_path)) {
    cli::cli_abort("No .Renviron file found at {.file {renviron_path}}")
  }
  
  # Clear specified variables from current session
  if (verbose) cli::cli_alert("Clearing environment variables")
  if (length(var_names) > 0) {
    for (var in var_names) {
      Sys.unsetenv(var)
    }
  }
  
  # Reload from .Renviron file
  readRenviron(renviron_path)
  if (verbose) cli::cli_alert_success("Synced environment variables from {.file {renviron_path}}")
  
  # Show current values of variables
  if (verbose) display_env_vars(var_names)
  
  return(invisible(NULL))
}
