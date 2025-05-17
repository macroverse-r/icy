#' Erase Variables from .Renviron File
#'
#' Removes specified environment variables from the user's .Renviron file.
#' Note: This only affects the .Renviron file, not the current R session.
#' Use sync_env_vars() after this to update the session.
#'
#' @param var_names Character vector of variable names to erase.
#' @param renviron_path Path to the .Renviron file.
#' @param validate Logical; if TRUE, validates variable names against the list from YAML.
#' @param allowed_vars Character vector of allowed variable names for validation.
#'   Only used if validate is TRUE.
#'   
#' @return Invisibly returns a character vector of the variables that were erased.
#' @export
erase_vars_from_renviron <- function(var_names,
                                   renviron_path = get_renviron_path(),
                                   validate = TRUE,
                                   allowed_vars = NULL) {
  
  # Validate variable names if requested
  if (validate) {
    validate_env_var_names(var_names, warn = FALSE, allowed_vars = allowed_vars)
  }
  
  # Make sure the file exists, but don't create it if it doesn't
  if (!file.exists(renviron_path)) {
    cli::cli_warn(".Renviron file not found at {.file {renviron_path}}")
    return(invisible(character(0)))
  }
  
  # Read existing content
  lines <- readLines(renviron_path, warn = FALSE)
  
  # Track which variables were erased for messaging
  erased_vars <- character(0)
  
  for (var_name in var_names) {
    # Create pattern to match the variable definition
    var_pattern <- paste0("^\\s*", var_name, "\\s*=")
    # Check if variable exists
    matches <- grep(var_pattern, lines)
    
    if (length(matches) > 0) {
      # Remove the variable from the file
      lines <- lines[-matches]
      erased_vars <- c(erased_vars, var_name)
    }
  }
  
  # Write back to file if any variables were erased
  if (length(erased_vars) > 0) {
    writeLines(lines, renviron_path)
    cli::cli_alert_success("Erased variables from .Renviron: {.val {erased_vars}}")
  }
  
  invisible(erased_vars)
}