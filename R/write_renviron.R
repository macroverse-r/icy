#' Write Environment Variables to .Renviron File
#'
#' This function writes or updates environment variables in the user's .Renviron file.
#' By default, it detects the calling package automatically and enables validation and grouping.
#' It can be used in two different ways:
#'
#' 1. With package detection (default) - Groups related variables, validates names
#' 2. With `package = NULL` (for general use) - Simple writing of variables without validation
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
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#'   If provided, enables validation and grouping related variables together in the .Renviron file. 
#'   Set to NULL to perform a simple write without validation or grouping.
#' @param section Character string. The section configuration to use. Defaults to "default".
#' @param renviron_path Path to the .Renviron file. Defaults to the user's home directory.
#' @param overwrite Logical; if TRUE (default), overwrites existing variables.
#'   If FALSE, existing variables are left unchanged.
#' @param validate Logical; if TRUE (default), validates variable names against the package's
#'   YAML configuration. Only applies when a package name is provided.
#' @param allowed_vars Optional character vector of allowed variable names for validation.
#'   If NULL (default), the function will use the names from the package's YAML configuration.
#'   Only applies when a package name is provided and validate=TRUE.
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
#' @param sync Character or logical. Controls session environment synchronization:
#'   - "conservative" (default): only sync variables already in session
#'   - "all" or TRUE: sync all written variables to session  
#'   - "none" or FALSE: skip synchronization
#'   - character vector: explicit list of variables to sync
#'   
#' @return Invisibly returns NULL on success.
#'
#' @examples
#' \dontrun{
#' # For package developers: Writing package variables to .Renviron
#' write_renviron(
#'   var_list = list(
#'     MY_PACKAGE_DATA_DIR = "/path/to/data",
#'     MY_PACKAGE_API_KEY = "secret-key"
#'   ),
#'   package = "mypackage"
#' )
#' 
#' # For general use: Setting environment variables without a package
#' write_renviron(
#'   var_list = list(
#'     R_MAX_VSIZE = "4GB",
#'     API_KEY = "my-api-key"
#'   ),
#'   package = NULL
#' )
#' 
#' # Using this function in a configuration utility for your package
#' configure_my_package <- function() {
#'   # Get user input for configuration variables
#'   data_dir <- readline("Enter data directory path: ")
#'   api_key <- readline("Enter API key: ")
#'   
#'   # Write to .Renviron
#'   write_renviron(
#'     var_list = list(
#'       MY_PACKAGE_DATA_DIR = data_dir,
#'       MY_PACKAGE_API_KEY = api_key
#'     ),
#'     package = "mypackage"
#'   )
#'   
#'   # Update current session
#'   sync(c("MY_PACKAGE_DATA_DIR", "MY_PACKAGE_API_KEY"))
#'   
#'   # Confirm to user
#'   cat("Configuration complete. Settings will be loaded in future R sessions.\n")
#' }
#' }
#'   
#' @export
write_renviron <- function(var_list,
                           package = get_package_name(),
                           section = "default",
                           renviron_path = get_renviron_path(),
                           overwrite = TRUE,
                           validate = TRUE,
                           allowed_vars = NULL,
                           verbose = FALSE,
                           sync = "conservative") {
  
  # Input validation
  if (!is.list(var_list) || length(var_list) == 0) {
    .icy_stop("var_list must be a non-empty named list of environment variables")
  }
  
  if (is.null(names(var_list)) || any(names(var_list) == "")) {
    .icy_stop("All elements in var_list must be named")
  }
  
  # Capture current session variables before any changes (only for package mode)
  original_session_vars <- character(0)
  if (!is.null(package)) {
    original_session_vars <- .get_current_session_vars(package, section)
  }
  
  # Delegate to the appropriate internal function based on whether a package is provided
  if (is.null(package)) {
    # Use the simple version for non-package variables
    result <- .write_simple_to_renviron(
      var_list = var_list,
      renviron_path = renviron_path,
      overwrite = overwrite,
      verbose = verbose
    )
  } else {
    # Use the package-specific version with validation and grouping
    result <- .write_pkg_to_renviron(
      var_list = var_list,
      package = package,
      section = section,
      renviron_path = renviron_path,
      overwrite = overwrite,
      validate = validate,
      verbose = verbose,
      allowed_vars = allowed_vars
    )
  }
  
  # Apply sync logic to session environment variables (only for package mode)
  if (length(result$written) > 0 && !is.null(package)) {
    synced_vars <- .apply_sync(var_list, sync, original_session_vars, verbose = verbose)
  }
  
  return(invisible(NULL))
}



# Internal function for writing variables not associated with a package
# Simply updates existing variables in-place or adds new ones at the end
.write_simple_to_renviron <- function(var_list, renviron_path, overwrite, verbose = FALSE) {
  # Ensure the .Renviron file exists
  if (!file.exists(renviron_path)) {
    if (!file.create(renviron_path)) {
      .icy_stop(paste0("Failed to create .Renviron file at ", renviron_path))
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
    if (verbose) {
      .icy_success(paste0("Wrote ", length(written_vars), " variable", if(length(written_vars) > 1) "s" else "", " to .Renviron: ", paste(written_vars, collapse = ", ")))
    }
  }
  
  # Report skipped variables
  if (length(existing_vars) > 0 && !overwrite && verbose) {
    .icy_alert(paste0("Skipped (overwrite=FALSE): ", paste(existing_vars, collapse = ", ")))
  }
  
  return(list(
    written = written_vars,
    existing = existing_vars
  ))
}

# Internal function for writing variables associated with a package
# Handles validation and groups variables together by package
.write_pkg_to_renviron <- function(var_list,
                                   package,
                                   section,
                                   renviron_path,
                                   overwrite,
                                   validate,
                                   verbose = FALSE,
                                   allowed_vars) {

  # Validate variable names if requested
  if (validate) {
    validate(package = package,
             var_names = names(var_list),
             warn = FALSE,
             allowed_vars = allowed_vars)
  }
  
  # Ensure the .Renviron file exists
  if (!file.exists(renviron_path)) {
    if (!file.create(renviron_path)) {
      .icy_stop(paste0("Failed to create .Renviron file at ", renviron_path))
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
    package_config <- get_config(package = package, section = section)
    package_var_names <- names(package_config)
  }, error = function(e) {
    # If we can't get the package var names, continue without grouping
    .icy_warn(paste0("Could not retrieve full list of package variables: ", e$message))
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
    if (verbose) {
      .icy_success(paste0("Wrote ", length(written_vars), " variable", if(length(written_vars) > 1) "s" else "", " to .Renviron: ", paste(written_vars, collapse = ", ")))
    }
  }
  
  # Report skipped variables
  if (length(existing_vars) > 0 && !overwrite && verbose) {
    .icy_alert(paste0("Skipped (overwrite=FALSE): ", paste(existing_vars, collapse = ", ")))
  }
  
  return(list(
    written = written_vars,
    existing = existing_vars
  ))
}
