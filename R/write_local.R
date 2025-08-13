#' Write Environment Variables to Local Configuration
#'
#' Writes or updates environment variables in the local configuration YAML file.
#' This function modifies the user's local configuration, preserving the YAML
#' structure and comments where possible.
#'
#' @param var_list Named list of environment variables to write. Names should be the
#'   environment variable names and values should be the values to set.
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param fn_local Character string with custom filename for the local config.
#'   If NULL, uses the default naming pattern.
#' @param create_if_missing Logical; if TRUE (default), creates the local config file
#'   from template if it doesn't exist. If FALSE, returns an error if file is missing.
#' @param case_format Character string indicating the case format to use for filenames.
#'   Options are: "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param fn_tmpl Character string with the name or path to a custom YAML template file
#'   for validation. If NULL (default), uses the standard template file for the package.
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
#' # Write variables to local config
#' write_local(
#'   var_list = list(
#'     API_KEY = "my-secret-key",
#'     DB_HOST = "localhost",
#'     DEBUG_MODE = "TRUE"
#'   ),
#'   package = "mypackage"
#' )
#'
#' # Write to a specific user section
#' write_local(
#'   var_list = list(API_URL = "https://prod.api.com"),
#'   package = "mypackage",
#'   section = "production"
#' )
#' }
#'
#' @export
write_local <- function(var_list,
                        package = get_package_name(),
                        section = "default",
                        fn_local = NULL,
                        create_if_missing = TRUE,
                        case_format = "snake_case",
                        fn_tmpl = NULL,
                        verbose = FALSE,
                        sync = "conservative") {
  # Input validation
  if (!is.list(var_list) || length(var_list) == 0) {
    .icy_stop("var_list must be a non-empty named list of environment variables")
  }

  if (is.null(names(var_list)) || any(names(var_list) == "")) {
    .icy_stop("All elements in var_list must be named")
  }
  
  # Capture current session variables before any changes
  original_session_vars <- .get_current_session_vars(package, section)
  
  # Validate against template configuration
  template_config <- tryCatch({
    if (is.null(fn_tmpl)) {
      get_config(package = package, origin = "template", section = section)
    } else {
      .get_config_template(package = package, section = section, yaml_file = fn_tmpl)
    }
  }, error = function(e) {
    .icy_stop(paste0("Could not read template configuration: ", e$message))
  })
  
  template_vars <- names(template_config)
  invalid_vars <- setdiff(names(var_list), template_vars)
  if (length(invalid_vars) > 0) {
    # Create a single, well-formatted error message
    error_msg <- paste0("Invalid variable name", if(length(invalid_vars) > 1) "s" else "", ": ", 
                       paste(invalid_vars, collapse = ", "), "\n\n",
                       "Valid template variables:\n  ", 
                       paste(template_vars, collapse = ", "))
    .icy_stop(error_msg)
  }

  # Find local config file
  local_path <- find_local(
    package = package,
    fn_local = fn_local,
    case_format = case_format,
    verbose = FALSE
  )

  # Create if missing
  if (is.null(local_path)) {
    if (create_if_missing) {
      local_path <- create_local(
        package = package,
        fn_local = fn_local,
        case_format = case_format
      )
      .icy_alert(paste0("Created new local config file: ", local_path))
    } else {
      .icy_stop(c(
        paste0("No local configuration file found for package ", package),
        "i" = "Set create_if_missing = TRUE to create one automatically"
      ))
    }
  }

  # Read existing configuration
  config_data <- yaml::read_yaml(local_path)

  # Ensure section exists
  if (!section %in% names(config_data)) {
    config_data[[section]] <- list()
  }

  # Track what we're updating and which vars should be NULL
  updated_vars <- character(0)
  new_vars <- character(0)
  null_vars <- names(var_list)[sapply(var_list, is.null)]

  # Update configuration
  for (var_name in names(var_list)) {
    old_value <- config_data[[section]][[var_name]]
    new_value <- var_list[[var_name]]
    
    # Set the value (R will remove NULL values, but we'll add them back)
    config_data[[section]][[var_name]] <- new_value

    if (!is.null(old_value)) {
      if (!identical(old_value, new_value)) {
        updated_vars <- c(updated_vars, var_name)
      }
    } else {
      new_vars <- c(new_vars, var_name)
    }
  }
  
  # Add back NULL values using a method that preserves them
  for (null_var in null_vars) {
    # Create a temporary list with the NULL value and merge it
    temp_list <- list(NULL)
    names(temp_list) <- null_var
    config_data[[section]] <- c(config_data[[section]], temp_list)
  }
  
  # Reorder configuration to match template order
  current_vars <- names(config_data[[section]])
  ordered_vars <- intersect(template_vars, current_vars)  # Template order, only existing vars
  config_data[[section]] <- config_data[[section]][ordered_vars]

  # Write back to file
  yaml::write_yaml(config_data, local_path)

  # Report what was done
  if (verbose) {
    if (length(updated_vars) > 0) {
      .icy_success(paste0("Updated ", length(updated_vars), " variable", if(length(updated_vars) > 1) "s" else "", " in local config"))
      bullets <- updated_vars
      names(bullets) <- rep("*", length(updated_vars))
      .icy_bullets(bullets)
    }

    if (length(new_vars) > 0) {
      .icy_success(paste0("Added ", length(new_vars), " new variable", if(length(new_vars) > 1) "s" else "", " to local config"))
      bullets <- new_vars
      names(bullets) <- rep("*", length(new_vars))
      .icy_bullets(bullets)
    }

    if (length(updated_vars) == 0 && length(new_vars) == 0) {
      .icy_alert("No changes made - all values were already up to date")
    }
  }

  # Apply sync logic to session environment variables
  if (length(updated_vars) > 0 || length(new_vars) > 0) {
    synced_vars <- .apply_sync(var_list, sync, original_session_vars, verbose = verbose)
  }

  return(invisible(NULL))
}

