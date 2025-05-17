#' Sync Variables from .Renviron to Current Session
#'
#' Makes the environment variables in the current R session match exactly
#' what's in the .Renviron file. This is the main function for ensuring consistency
#' between the .Renviron file and the active session.
#'
#' @param var_names Character vector of variable names to sync.
#'   If NULL (default), uses all variables from the YAML config.
#' @param verbose Whether to display status messages and current values.
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' @export
sync_env_vars <- function(var_names = NULL, verbose = TRUE) {
  # Internal function to clear environment variables
  clear_env_vars <- function(vars) {
    if (length(vars) > 0) {
      for (var in vars) {
        Sys.unsetenv(var)
      }
    }
  }
  
  # If no var_names provided, get all from YAML
  if (is.null(var_names)) {
    var_names <- get_env_var_names()
  }
  
  # Get path to .Renviron file
  renviron_path <- get_renviron_path()
  
  # Check file exists
  if (!file.exists(renviron_path)) {
    cli::cli_warn("No .Renviron file found at {.file {renviron_path}}")
    return(invisible(FALSE))
  }
  
  # Clear specified variables from current session 
  if (verbose) cli::cli_alert("Clearing environment variables")
  clear_env_vars(var_names)
  
  # Reload from .Renviron file
  readRenviron(renviron_path)
  if (verbose) cli::cli_alert_success("Synced environment variables from {.file {renviron_path}}")
  
  # Show current values of variables
  if (verbose) display_env_vars(var_names)
  
  invisible(TRUE)
}