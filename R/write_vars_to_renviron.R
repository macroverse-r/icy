#' Write Package Environment Variables to .Renviron File
#'
#' This function writes or updates environment variables in the user's .Renviron file.
#' It is designed to be used within R packages to help manage package-specific 
#' environment variables in a consistent and user-friendly way.
#'
#' Key features:
#' 
#' * Updates existing variables in-place, preserving their position in the .Renviron file
#' * Groups related variables from the same package together in the file
#' * Validates variable names against the package's YAML configuration
#' * Provides informative messages about the changes made
#'
#' Note: This function only modifies the .Renviron file, not the current R session.
#' Use `sync_env_vars()` after calling this function to update the current session.
#'
#' @param var_list Named list of environment variables to write. Names should be the
#'   environment variable names and values should be the values to set.
#' @param package Character string with the package name. Used both for validation
#'   and for grouping related variables together in the .Renviron file.
#' @param renviron_path Path to the .Renviron file. Defaults to the user's home directory.
#' @param overwrite Logical; if TRUE (default), overwrites existing variables.
#'   If FALSE, existing variables are left unchanged.
#' @param validate Logical; if TRUE (default), validates variable names against the package's
#'   YAML configuration. Set to FALSE to skip validation.
#' @param allowed_vars Optional character vector of allowed variable names for validation.
#'   If NULL (default), the function will use the names from the package's YAML configuration.
#'   
#' @return Returns TRUE invisibly on success.
#'
#' @examples
#' \dontrun{
#' # For package developers: Writing package variables to .Renviron
#' write_vars_to_renviron(
#'   var_list = list(
#'     MY_PACKAGE_DATA_DIR = "/path/to/data",
#'     MY_PACKAGE_API_KEY = "secret-key"
#'   ),
#'   package = "mypackage"
#' )
#' 
#' # Using this function in a configuration utility for your package
#' configure_my_package <- function() {
#'   # Get user input for configuration variables
#'   data_dir <- readline("Enter data directory path: ")
#'   api_key <- readline("Enter API key: ")
#'   
#'   # Write to .Renviron
#'   write_vars_to_renviron(
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
write_vars_to_renviron <- function(var_list,
                                   package = NULL,
                                   renviron_path = get_renviron_path(),
                                   overwrite = TRUE,
                                   validate = TRUE,
                                   allowed_vars = NULL) {

  # Validate variable names if requested
  if (validate && !is.null(package)) {
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



# @CLAUDE: make it so htat it is also possible to use it like: yml2renv::write_vars_to_renviron(list(API_KEY = "MY_API_KEY")) to the .Renviron independently of any package.
# Keep a clear yet concise explanation of the implication of additional the package name
# To make things clearer, make two subfunctions in this file, one for the case with package provided and one for the case without package provided. These two sub functions are only internal functions
# If the package is provided, it should act like now.
# if pacjage is not provided, it should check if the env variable already exist. If it does not exist, add it at the end of the .Renviron (new line). If is exist, overwrite it at the same location in the .Renviron if overwrite is TRUE


