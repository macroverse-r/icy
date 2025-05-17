#' Find Environment Variables YAML File
#'
#' Searches for YAML files that match the pattern "*env_vars.yml" or similar
#' based on the requested case format. The function searches within the package
#' installation directory, including all subdirectories.
#'
#' @param case_format Character string indicating the case format to use.
#'   Options are: "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param package Character string with the package name to search within. If NULL (default),
#'   the function will attempt to determine the current package name automatically.
#'
#' @return Character string with the full path to the found YAML file.
#' @export
get_env_vars_yaml <- function(case_format = "snake_case", package = NULL) {
  # Determine package name automatically if not provided
  if (is.null(package)) {
    package <- get_package_name()
  }
  
  # Define search patterns based on case format
  pattern <- switch(case_format,
                    "snake_case" = "_env_vars\\.ya?ml$",
                    "camelCase" = "EnvVars\\.ya?ml$",
                    "PascalCase" = "EnvVars\\.ya?ml$",
                    "kebab-case" = "-env-vars\\.ya?ml$",
                    # Default to snake_case if unknown format
                    "_env_vars\\.ya?ml$")
  
  # Get the package path
  tryCatch({
    pkg_path <- system.file(package = package)
    
    # Special handling for devtools loaded packages
    if (endsWith(pkg_path, "/inst")) {
      # For development mode, search from parent directory
      search_path <- dirname(pkg_path)
    } else {
      # For installed packages, start with the package directory
      search_path <- pkg_path
    }
    
    # Find all .yml or .yaml files in the package
    tryCatch({
      # Use list.files to recursively find yaml files
      yaml_files <- list.files(
        path = search_path,
        pattern = "\\.ya?ml$",
        recursive = TRUE,
        full.names = TRUE
      )
      
      # Filter for files matching our pattern
      matching_files <- yaml_files[grepl(pattern, yaml_files, ignore.case = TRUE)]
      
      if (length(matching_files) == 0) {
        stop("No environment variables YAML file found in ", search_path, 
             " matching pattern '", pattern, "'")
      } 
      
      if (length(matching_files) > 1) {
        stop("Multiple environment variables YAML files found: ", 
             paste(basename(matching_files), collapse = ", "), 
             ". Please ensure only one file is present.")
      }
      
      return(as.character(matching_files))
      
    }, error = function(e) {
      if (grepl("cannot open|no such file", e$message, ignore.case = TRUE)) {
        stop("Directory not found: ", search_path)
      } else {
        stop("Error searching for YAML files: ", e$message)
      }
    })
    
  }, error = function(e) {
    stop("Error locating package path: ", e$message)
  })
}