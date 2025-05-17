#' Write Environment Variables to .Renviron File
#'
#' This function writes or updates environment variables in the user's .Renviron file.
#' It can be used in two different ways:
#'
#' 1. With a package name (for package developers) - Groups related variables, validates names
#' 2. Without a package name (for general use) - Simple writing of variables
#'
#' Key features:
#' 
#' * Updates existing variables in-place, preserving their position in the .Renviron file
#' * When a package name is provided, groups related variables together in the file
#' * When a package name is provided, validates variable names against the package's YAML configuration
#' * Provides informative messages about the changes made
#'
#' Note: This function only modifies the .Renviron file, not the current R session.
#' Use `sync_env_vars()` after calling this function to update the current session.
#'
#' @param var_list Named list of environment variables to write. Names should be the
#'   environment variable names and values should be the values to set.
#' @param package Character string with the package name. If provided, enables validation
#'   and grouping related variables together in the .Renviron file. If NULL (default),
#'   performs a simple write without validation or grouping.
#' @param renviron_path Path to the .Renviron file. Defaults to the user's home directory.
#' @param overwrite Logical; if TRUE (default), overwrites existing variables.
#'   If FALSE, existing variables are left unchanged.
#' @param validate Logical; if TRUE (default), validates variable names against the package's
#'   YAML configuration. Only applies when a package name is provided.
#' @param allowed_vars Optional character vector of allowed variable names for validation.
#'   If NULL (default), the function will use the names from the package's YAML configuration.
#'   Only applies when a package name is provided and validate=TRUE.
#'   
#' @return Returns TRUE invisibly on success.
#'
#' @examples
#' \dontrun{
#' # For package developers: Writing package variables to .Renviron
#' write_to_renviron(
#'   var_list = list(
#'     MY_PACKAGE_DATA_DIR = "/path/to/data",
#'     MY_PACKAGE_API_KEY = "secret-key"
#'   ),
#'   package = "mypackage"
#' )
#' 
#' # For general use: Setting environment variables without a package
#' write_to_renviron(
#'   var_list = list(
#'     R_MAX_VSIZE = "4GB",
#'     API_KEY = "my-api-key"
#'   )
#' )
#' 
#' # Using this function in a configuration utility for your package
#' configure_my_package <- function() {
#'   # Get user input for configuration variables
#'   data_dir <- readline("Enter data directory path: ")
#'   api_key <- readline("Enter API key: ")
#'   
#'   # Write to .Renviron
#'   write_to_renviron(
#'     var_list = list(
#'       MY_PACKAGE_DATA_DIR = data_dir,
#'       MY_PACKAGE_API_KEY = api_key
#'     ),
#'     package = "mypackage"
#'   )
#'   
#'   # Update current session
#'   sync_env_vars(c("MY_PACKAGE_DATA_DIR", "MY_PACKAGE_API_KEY"))
#'   
#'   # Confirm to user
#'   cat("Configuration complete. Settings will be loaded in future R sessions.\n")
#' }
#' }
#'   
#' @export
write_to_renviron <- function(var_list,
                              package = NULL,
                              renviron_path = get_renviron_path(),
                              overwrite = TRUE,
                              validate = TRUE,
                              allowed_vars = NULL) {
  
  # Input validation
  if (!is.list(var_list) || length(var_list) == 0) {
    cli::cli_abort("var_list must be a non-empty named list of environment variables")
  }
  
  if (is.null(names(var_list)) || any(names(var_list) == "")) {
    cli::cli_abort("All elements in var_list must be named")
  }
  
  # Delegate to the appropriate internal function based on whether a package is provided
  if (is.null(package)) {
    # Use the simple version for non-package variables
    .write_simple_to_renviron(
      var_list = var_list,
      renviron_path = renviron_path,
      overwrite = overwrite
    )
  } else {
    # Use the package-specific version with validation and grouping
    .write_pkg_to_renviron(
      var_list = var_list,
      package = package,
      renviron_path = renviron_path,
      overwrite = overwrite,
      validate = validate, 
      allowed_vars = allowed_vars
    )
  }
  
  return(invisible(TRUE))
}



# Internal function for writing variables not associated with a package
# Simply updates existing variables in-place or adds new ones at the end
.write_simple_to_renviron <- function(var_list, renviron_path, overwrite) {
  # Ensure the .Renviron file exists
  if (!file.exists(renviron_path)) {
    if (!file.create(renviron_path)) {
      cli::cli_abort("Failed to create .Renviron file at {.file {renviron_path}}")
    }
  }
  
  # Read existing content
  lines <- readLines(renviron_path, warn = FALSE)
  
  # Track which variables we're working with
  written_vars <- character(0)
  existing_vars <- character(0)
  
  # Process existing variables first - update in-place
  for (var_name in names(var_list)) {
    var_pattern <- paste0("^\\s*", var_name, "\\s*=")
    var_matches <- grepl(var_pattern, lines)
    
    if (any(var_matches)) {
      existing_vars <- c(existing_vars, var_name)
      
      # If overwriting, update the existing line in-place
      if (overwrite) {
        match_idx <- which(var_matches)[1]
        lines[match_idx] <- paste0(var_name, "=", var_list[[var_name]])
        written_vars <- c(written_vars, var_name)
      }
    }
  }
  
  # Now handle new variables - simply append them to the end
  new_vars <- setdiff(names(var_list), existing_vars)
  if (length(new_vars) > 0) {
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
  
  return(list(
    written = written_vars,
    existing = existing_vars
  ))
}

# Internal function for writing variables associated with a package
# Handles validation and groups variables together by package
.write_pkg_to_renviron <- function(var_list, package, renviron_path, overwrite, validate, allowed_vars) {
  # Validate variable names if requested
  if (validate) {
    validate_env_var_names(var_names = names(var_list),
                         package = package,
                         warn = FALSE,
                         allowed_vars = allowed_vars)
  }
  
  # Ensure the .Renviron file exists
  if (!file.exists(renviron_path)) {
    if (!file.create(renviron_path)) {
      cli::cli_abort("Failed to create .Renviron file at {.file {renviron_path}}")
    }
  }
  
  # Read existing content
  lines <- readLines(renviron_path, warn = FALSE)
  
  # Track which variables we're working with
  written_vars <- character(0)
  existing_vars <- character(0)
  
  # Get all environment variables for this package
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
        match_idx <- which(var_matches)[1]
        lines[match_idx] <- paste0(var_name, "=", var_list[[var_name]])
        written_vars <- c(written_vars, var_name)
      }
    }
  }
  
  # Process new variables that need to be added
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
  
  return(list(
    written = written_vars,
    existing = existing_vars
  ))
}
