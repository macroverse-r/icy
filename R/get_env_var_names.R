#' Get Environment Variable Names
#'
#' Reads the list of environment variable names from the YAML config file.
#' If the YAML file name is not provided, it will search for a file using the
#' `get_env_vars_yaml` function with the specified case format.
#'
#' @param yaml_file Character string with the name of the YAML file. If NULL,
#'   the function will search for a file using `get_env_vars_yaml`.
#' @param package Character string with the package name. If NULL (default),
#'   the function will attempt to determine the current package name automatically.
#' @param case_format Character string indicating the case format to use for
#'   searching the YAML file if `yaml_file` is NULL. Options are:
#'   "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#'
#' @return Character vector of environment variable names.
#' @export
get_env_var_names <- function(yaml_file = NULL, package = NULL, case_format = "snake_case") {
  # Determine package name automatically if not provided
  if (is.null(package)) {
    package <- get_current_package_name()
  }
  
  # Find the YAML file
  if (is.null(yaml_file)) {
    # Try to find the YAML file using get_env_vars_yaml
    tryCatch({
      yaml_file <- get_env_vars_yaml(case_format, package)
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
      yaml_file <<- fallback_file
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