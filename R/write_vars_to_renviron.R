#' Write Variables to .Renviron File
#'
#' Writes environment variables to the user's .Renviron file.
#' By default, overwrites variables that already exist.
#' Note: This only affects the .Renviron file, not the current R session.
#' Use sync_env_vars() after this to update the session.
#'
#' @param var_list Named list of environment variables to write.
#' @param renviron_path Path to the .Renviron file.
#' @param overwrite Logical; if TRUE, overwrites existing variables. Default is TRUE.
#' @param validate Logical; if TRUE, validates variable names against the list from YAML.
#' @param allowed_vars Character vector of allowed variable names for validation.
#'   Only used if validate is TRUE.
#'   
#' @return Invisibly returns a character vector of the variables that were written.
#' @export
write_vars_to_renviron <- function(var_list,
                                 renviron_path = get_renviron_path(),
                                 overwrite = TRUE,
                                 validate = TRUE,
                                 allowed_vars = NULL) {

  # Validate variable names if requested
  if (validate) {
    validate_env_var_names(names(var_list), warn = FALSE, allowed_vars = allowed_vars)
  }

  # Checks if the .Renviron file exists and creates it if it doesn't
  if (!file.exists(renviron_path)) {
    if (!file.create(renviron_path)) {
      cli::cli_abort("Failed to create .Renviron file at {.file {renviron_path}}")
    }
  }
  
  # Read existing content
  lines <- readLines(renviron_path, warn = FALSE)
  
  # Track which variables were newly written for messaging
  written_vars <- character(0)
  existing_vars <- character(0)
  
  for (var_name in names(var_list)) {
    # Check if variable exists in .Renviron file
    var_pattern <- paste0("^\\s*", var_name, "\\s*=")
    
    if (any(grepl(var_pattern, lines))) {
      existing_vars <- c(existing_vars, var_name)
      
      # If overwriting, remove old definition and add new one
      if (overwrite) {
        lines <- lines[!grepl(var_pattern, lines)]
        lines <- c(lines, paste0(var_name, "=", var_list[[var_name]]))
        written_vars <- c(written_vars, var_name)
      }
    } else {
      # Add new variable
      lines <- c(lines, paste0(var_name, "=", var_list[[var_name]]))
      written_vars <- c(written_vars, var_name)
    }
  }
  
  # Write back to file if any variables were written
  if (length(written_vars) > 0) {
    writeLines(lines, renviron_path)
    cli::cli_alert_success("Wrote to .Renviron: {.val {written_vars}}")
  }
  
  # Report skipped variables
  if (length(existing_vars) > 0 && !overwrite) {
    cli::cli_alert_info("Skipped (overwrite=FALSE): {.val {existing_vars}}")
  }
  
  invisible(written_vars)
}