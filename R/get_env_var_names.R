#' Get Environment Variable Names from YAML Config
#'
#' Reads the list of environment variable names from a package's YAML configuration file.
#' This function is typically used within an R package to retrieve the list of environment
#' variables the package uses, as defined in a YAML configuration file.
#'
#' If the YAML file name is not provided, the function will search for a file using the
#' `get_env_vars_yaml` function with the specified case format. The YAML file should 
#' contain an `environment_variables` list with the names of the environment variables.
#'
#' @param package Character string with the package name. Either this or `yaml_file` 
#'   must be provided.
#' @param yaml_file Character string with the name or path to the YAML file. If NULL,
#'   the function will search for a YAML file ending with `env_vars.yml` using `get_env_vars_yaml`.
#'   Either this or `package` must be provided.
#' @param case_format Character string indicating the case format to use for
#'   searching the YAML file if `yaml_file` is NULL. Options are:
#'   "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#'
#' @return Character vector of environment variable names.
#' 
#' @examples
#' \dontrun{
#' # Inside a package named "mypackage" with a YAML file at inst/mypackage_env_vars.yml:
#' # Get all environment variables defined for the package
#' var_names <- get_env_var_names(package = "mypackage")
#' 
#' # Use these names for validation or to retrieve current values
#' current_values <- lapply(var_names, Sys.getenv, unset = NA)
#' names(current_values) <- var_names
#' 
#' # With (or without) a custom-named YAML file
#' var_names <- get_env_var_names(
#'   yaml_file = "inst/environment.yml"
#' )
#' }
#' 
#' @export
get_env_var_names <- function(package = NULL, yaml_file = NULL, case_format = "snake_case") {
  
  # Check that at least one of package or yaml_file is provided
  if (is.null(package) && is.null(yaml_file)) {
    stop("At least one of 'package' or 'yaml_file' must be provided")
  }

  # Find the YAML file
  if (is.null(yaml_file)) {
    # Try to find the YAML file using get_env_vars_yaml
    # Try primary method first (get_env_vars_yaml)
    yaml_file_found <- FALSE
    
    # Try using get_env_vars_yaml
    tryCatch({
      yaml_file <- get_env_vars_yaml(package, case_format)
      yaml_file_found <- TRUE
    }, error = function(e) {
      # Error will be handled in the following fallback methods
    })
    
    # If primary method failed, try fallback methods
    if (!yaml_file_found) {
      # Fallback 1: Try [package]_env_vars.yml
      potential_file <- system.file(paste0(package, "_env_vars.yml"), package = package)
      if (potential_file != "") {
        yaml_file <- potential_file
      } else {
        # Fallback 2: Try generic env_vars.yml
        potential_file <- system.file("env_vars.yml", package = package)
        if (potential_file != "") {
          yaml_file <- potential_file
        } else {
          # All methods failed
          stop("Could not find environment variables YAML file for package '", package, "'")
        }
      }
    }
  } else {
    # If yaml_file is provided, check if it's a file path or just a file name
    if (!file.exists(yaml_file)) {
      if (!is.null(package)) {
        # Try to find it in the package
        yaml_file <- system.file(yaml_file, package = package)
        if (yaml_file == "") {
          stop("YAML file '", yaml_file, "' not found in package '", package, "'")
        }
      } else {
        stop("YAML file '", yaml_file, "' does not exist")
      }
    }
  }
  
  # Read the YAML file
  tryCatch({
    # Use the yaml package's read function
    env_vars <- yaml::read_yaml(yaml_file)$environment_variables
    if (is.null(env_vars) || length(env_vars) == 0) {
      stop("No environment variables found in YAML file at ", yaml_file)
    }
  }, error = function(e) {
    stop("Error reading YAML file: ", e$message)
  })

  return(env_vars)

}
