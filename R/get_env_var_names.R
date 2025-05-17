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
#' @param package Character string with the package name.
#' @param yaml_file Character string with the name of the YAML file. If NULL,
#'   the function will search for a YAML file ending with `env_vars.yml` using `get_env_vars_yaml`.
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
  
  # @CLAUDE: add here to test that either package or yaml_file is provided (one or the other, or both). If none, provide an informative error. Adjust documentation accordingly
  # adjust the rest of the function to make sure it work if package is provided only and if yaml_file is provided only.

  # Find the YAML file
  if (is.null(yaml_file)) {
    # Try to find the YAML file using get_env_vars_yaml
    tryCatch({
      yaml_file <- get_env_vars_yaml(package, case_format)
    }, error = function(e) {
      # Fall back to the original method if the search fails
      fallback_file <- system.file(paste0(package, "_env_vars.yml"), package = package)
      if (fallback_file == "") {
        # Try a generic env_vars.yml
        fallback_file <- system.file("env_vars.yml", package = package)
        if (fallback_file == "") {
          stop("Could not find environment variables YAML file. Error: ", e$message)
        }
      }
      yaml_file <- fallback_file
    })
  } else {
    # If yaml_file is provided, check if it's a file path or just a file name
    if (!file.exists(yaml_file)) {
      # Try to find it in the package
      yaml_file <- system.file(yaml_file, package = package)
      if (yaml_file == "") {
        stop("YAML file '", yaml_file, "' not found in package '", package, "'")
      }
    }
  }
  
  # Read the YAML file
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The yaml package is required but not available. Please install it with: install.packages('yaml')")
  }
  
  tryCatch({
    # Use the yaml package's read function
    env_vars <- yaml::read_yaml(yaml_file)$environment_variables
    if (is.null(env_vars) || length(env_vars) == 0) {
      stop("No environment variables found in YAML file at ", yaml_file)
    }
    return(env_vars)
  }, error = function(e) {
    stop("Error reading YAML file: ", e$message)
  })
}
