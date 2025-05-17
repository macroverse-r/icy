#' Write Variables to .Renviron File
#'
#' Writes environment variables to the user's .Renviron file.
#' By default, overwrites variables that already exist in-place.
#' Note: This only affects the .Renviron file, not the current R session.
#' Use sync_env_vars() after this to update the session.
#'
#' For existing variables, this function updates their values in-place,
#' preserving their position in the file. For new variables, it attempts to
#' group them with other variables from the same package, by adding them
#' after the last existing variable from that package.
#'
#' @param var_list Named list of environment variables to write.
#' @param package Character string with the package name. Used for validation
#'   and for grouping related variables together in the .Renviron file.
#' @param renviron_path Path to the .Renviron file.
#' @param overwrite Logical; if TRUE, overwrites existing variables. Default is TRUE.
#' @param validate Logical; if TRUE, validates variable names against the list from YAML.
#' @param allowed_vars Character vector of allowed variable names for validation.
#'   Only used if validate is TRUE.
#'   
#' @export
write_vars_to_renviron <- function(var_list,
                                   package,
                                   renviron_path = get_renviron_path(),
                                   overwrite = TRUE,
                                   validate = TRUE,
                                   allowed_vars = NULL) {

  # Validate variable names if requested
  if (validate) {
    validate_env_var_names(var_names = names(var_list),
                           package = package,
                           warn = FALSE,
                           allowed_vars = allowed_vars)
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
  
  # First, get all environment variables for this package
  package_var_names <- character(0)
  tryCatch({
    package_var_names <- get_env_var_names(package = package)
  }, error = function(e) {
    # If we can't get the package var names, continue without grouping
    cli::cli_warn("Could not retrieve full list of package variables: {e$message}")
  })
  
  # Process existing variables first (in-place updates)
  for (var_name in names(var_list)) {
    var_pattern <- paste0("^\\s*", var_name, "\\s*=")
    var_matches <- grepl(var_pattern, lines)
    
    if (any(var_matches)) {
      existing_vars <- c(existing_vars, var_name)
      
      # If overwriting, modify the existing line in-place
      if (overwrite) {
        # Find the first match and update it
        match_idx <- which(var_matches)[1]
        lines[match_idx] <- paste0(var_name, "=", var_list[[var_name]])
        written_vars <- c(written_vars, var_name)
      }
    }
  }
  
  # Now process new variables that need to be added
  new_vars <- setdiff(names(var_list), existing_vars)
  
  if (length(new_vars) > 0 && length(package_var_names) > 0) {
    # For each new variable, find if any of the package variables already exist in .Renviron
    for (var_name in new_vars) {
      # Find all existing variables from this package in the file
      package_vars_in_file <- character(0)
      package_var_indices <- integer(0)
      
      for (pkg_var in package_var_names) {
        pkg_pattern <- paste0("^\\s*", pkg_var, "\\s*=")
        pkg_matches <- grepl(pkg_pattern, lines)
        
        if (any(pkg_matches)) {
          package_vars_in_file <- c(package_vars_in_file, pkg_var)
          package_var_indices <- c(package_var_indices, which(pkg_matches)[1])
        }
      }
      
      if (length(package_var_indices) > 0) {
        # If we found existing package variables, add this one after the last one
        last_index <- max(package_var_indices)
        
        # Insert the new variable after the last related package variable
        lines <- c(
          lines[1:last_index],
          paste0(var_name, "=", var_list[[var_name]]),
          if (last_index < length(lines)) lines[(last_index+1):length(lines)] else character(0)
        )
      } else {
        # If no existing package variables, just append to the end
        lines <- c(lines, paste0(var_name, "=", var_list[[var_name]]))
      }
      
      written_vars <- c(written_vars, var_name)
    }
  } else if (length(new_vars) > 0) {
    # If we don't have package_var_names, just append all new variables
    for (var_name in new_vars) {
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
  
  return(invisible(TRUE))
}
