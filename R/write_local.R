#' Write Environment Variables to Local Configuration
#'
#' Writes or updates environment variables in the local configuration YAML file.
#' This function modifies the user's local configuration, preserving the YAML
#' structure and comments where possible.
#'
#' @param var_list Named list of environment variables to write. Names should be the
#'   environment variable names and values should be the values to set.
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param fn_local Character string with custom filename for the local config.
#'   If NULL, uses the default naming pattern.
#' @param create_if_missing Logical; if TRUE (default), creates the local config file
#'   from template if it doesn't exist. If FALSE, returns an error if file is missing.
#' @param case_format Character string indicating the case format to use for filenames.
#'   Options are: "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
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
#'   user = "production"
#' )
#' }
#'
#' @export
write_local <- function(var_list,
                        package = get_package_name(),
                        user = "default",
                        fn_local = NULL,
                        create_if_missing = TRUE,
                        case_format = "snake_case",
                        verbose = FALSE) {
  # Input validation
  if (!is.list(var_list) || length(var_list) == 0) {
    icy_abort("var_list must be a non-empty named list of environment variables")
  }

  if (is.null(names(var_list)) || any(names(var_list) == "")) {
    icy_abort("All elements in var_list must be named")
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
      icy_alert_info(paste0("Created new local config file: ", local_path))
    } else {
      icy_abort(c(
        paste0("No local configuration file found for package ", package),
        "i" = "Set create_if_missing = TRUE to create one automatically"
      ))
    }
  }

  # Read existing configuration
  config_data <- yaml::read_yaml(local_path)

  # Ensure user section exists
  if (!user %in% names(config_data)) {
    config_data[[user]] <- list()
  }

  # Track what we're updating
  updated_vars <- character(0)
  new_vars <- character(0)

  # Update configuration
  for (var_name in names(var_list)) {
    old_value <- config_data[[user]][[var_name]]
    new_value <- var_list[[var_name]]
    config_data[[user]][[var_name]] <- new_value

    if (!is.null(old_value)) {
      if (old_value != new_value) {
        updated_vars <- c(updated_vars, var_name)
      }
    } else {
      new_vars <- c(new_vars, var_name)
    }
  }

  # Write back to file
  yaml::write_yaml(config_data, local_path)

  # Report what was done
  if (verbose) {
    if (length(updated_vars) > 0) {
      icy_alert_success(paste0("Updated ", length(updated_vars), " variable", if(length(updated_vars) > 1) "s" else "", " in local config"))
      bullets <- updated_vars
      names(bullets) <- rep("*", length(updated_vars))
      icy_bullets(bullets)
    }

    if (length(new_vars) > 0) {
      icy_alert_success(paste0("Added ", length(new_vars), " new variable", if(length(new_vars) > 1) "s" else "", " to local config"))
      bullets <- new_vars
      names(bullets) <- rep("*", length(new_vars))
      icy_bullets(bullets)
    }

    if (length(updated_vars) == 0 && length(new_vars) == 0) {
      icy_alert_info("No changes made - all values were already up to date")
    }
  }

  return(invisible(NULL))
}

