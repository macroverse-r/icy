#' Load Configuration into Environment Variables
#'
#' Loads configuration from various sources into the current R session using `Sys.setenv()`.
#' This function combines configuration from multiple sources with fallback defaults to ensure
#' all required environment variables are set.
#'
#' @description
#' The function follows this priority order for setting environment variables:
#' 1. Values from the specified origin (default: "priority" which means .Renviron > local config)
#' 2. Template values for variables not found in the origin
#' 3. Unset defaults for variables not found in either origin or template
#'
#' This ensures that all variables defined in the template are loaded into the environment,
#' with appropriate fallbacks when values are missing from higher-priority sources.
#'
#' @param package Character string with the package name. If NULL (default),
#'   uses the current package name.
#' @param origin Character string specifying where to read the primary configuration from:
#'   - "template": Read from the package's template YAML file (read-only blueprint)
#'   - "local": Read from the user's local configuration file
#'   - "renviron": Read from .Renviron file
#'   - "priority": Read with priority order (.Renviron > local config, default)
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param unset Named list of default values to use for variables that are not set
#'   in any configuration source. Names should match template variable names.
#' @param ensure_local Logical. If TRUE (default), creates a local configuration file 
#'   if it doesn't exist. Only applies when origin includes local config.
#' @param yaml_file Character string with the name or path to the YAML file. If NULL,
#'   the function will search for the appropriate file based on the origin.
#' @param case_format Character string indicating the case format to use for
#'   searching the YAML file if `yaml_file` is NULL. Options are:
#'   "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
#'
#' @return Invisibly returns a named list of the environment variables that were set.
#'
#' @examples
#' \dontrun{
#' # Load configuration with defaults for missing variables
#' load_config(
#'   package = "dummy",
#'   unset = list(DUMMY_VERBOSE = TRUE, DUMMY_DEBUG = FALSE)
#' )
#'
#' # Load from local config only with fallbacks
#' load_config(
#'   package = "mypackage",
#'   origin = "local",
#'   unset = list(MYPACKAGE_TIMEOUT = 30)
#' )
#'
#' # Load configuration for current package
#' load_config(unset = list(API_KEY = "default-key"))
#' }
#'
#' @export
load_config <- function(package = get_package_name(),
                        origin = "priority",
                        user = "default",
                        unset = list(),
                        ensure_local = TRUE,
                        yaml_file = NULL,
                        case_format = "snake_case",
                        verbose = FALSE) {

  # Input validation
  if (!is.list(unset)) {
    .icy_abort("unset must be a named list")
  }

  # Get template configuration to know all possible variables
  template_config <- tryCatch({
    get_config(
      package = package,
      origin = "template",
      user = "default",
      yaml_file = yaml_file,
      case_format = case_format
    )
  }, error = function(e) {
    .icy_abort(paste0("Failed to read template configuration: ", e$message))
  })

  # Validate that unset names match template variable names
  if (length(unset) > 0) {
    template_var_names <- names(template_config)
    unset_var_names <- names(unset)
    
    if (is.null(unset_var_names) || any(unset_var_names == "")) {
      .icy_abort("All elements in unset must be named")
    }
    
    invalid_names <- setdiff(unset_var_names, template_var_names)
    if (length(invalid_names) > 0) {
      .icy_abort(c(
        paste0("Invalid variable names in unset: ", paste(invalid_names, collapse = ", ")),
        "i" = paste0("Valid template variables: ", paste(template_var_names, collapse = ", "))
      ))
    }
  }

  # Create local config if needed and requested
  if (ensure_local && origin %in% c("local", "priority")) {
    # Check if local config exists
    local_file <- tryCatch({
      find_local(package = package, case_format = case_format)
    }, error = function(e) NULL)
    
    if (is.null(local_file)) {
      # Create local config from template
      tryCatch({
        create_local(
          package = package,
          tmpl_section = "default",
          case_format = case_format,
          overwrite = FALSE
        )
      }, error = function(e) {
        .icy_abort(paste0("Failed to create local config: ", e$message))
      })
    }
  }

  # Get configuration from specified origin (this already handles priority merging)
  config <- tryCatch({
    get_config(
      package = package,
      origin = origin,
      user = user,
      yaml_file = yaml_file,
      case_format = case_format
    )
  }, error = function(e) {
    .icy_abort(paste0("Failed to read ", origin, " configuration: ", e$message))
  })

  # Add missing variables using unset defaults or template values
  for (var_name in names(template_config)) {
    not_configured <- !var_name %in% names(config) || is.null(config[[var_name]])
    if (not_configured && var_name %in% names(unset)) {
      config[[var_name]] <- unset[[var_name]]
    }
  }

  # Remove empty/null entries to avoid error
  clean_config <- config[sapply(config, function(x) !is.null(x) && length(x) > 0)]

  if (length(config) > 0) {
    # Set the environment variables
    do.call(Sys.setenv, clean_config)
    
    # Show success message
    if (verbose) {
      var_names <- names(config)
      .icy_alert_success(paste0("Loaded ", length(var_names), " env. variable", if(length(var_names) > 1) "s" else "", ": ", paste(var_names, collapse = ", ")))
    }
  } else {
    if (verbose) .icy_alert_info("No environment variables to load")
  }

  # Return the configuration invisibly
  return(invisible(config))
}

