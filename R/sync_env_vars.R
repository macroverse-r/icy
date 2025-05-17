#' Sync Variables from .Renviron to Current Session
#'
#' Makes the environment variables in the current R session match exactly
#' what's in the .Renviron file. This is the main function for ensuring consistency
#' between the .Renviron file and the active session.
#'
#' @param var_names Character vector of variable names to sync.
#' @param verbose Whether to display status messages and current values.
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' @export
sync_env_vars <- function(var_names, verbose = TRUE) {
  
  # Get path to .Renviron file
  renviron_path <- get_renviron_path()
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
