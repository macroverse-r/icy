#' Find Environment Variables YAML Configuration File
#'
#' Locates the YAML configuration file that contains environment variable definitions
#' for a specified package. The function searches within the package's installation directory
#' (or development directory for packages loaded with devtools), including all subdirectories.
#'
#' This function is primarily designed for package developers who want to define and manage
#' environment variables for their package in a structured way. It supports different naming
#' conventions for the YAML files to accommodate various coding styles.
#'
#' The function automatically handles the difference between installed packages and packages
#' loaded via devtools during development.
#'
#' @param package Character string with the package name to search within.
#' @param case_format Character string indicating the case format to use for the filename search.
#'   Options are: 
#'   \itemize{
#'     \item "snake_case" (default): Searches for files like "package_env_vars.yml" or "*_env_vars.yml"
#'     \item "camelCase" or "PascalCase": Searches for files like "packageEnvVars.yml" or "*EnvVars.yml"
#'     \item "kebab-case": Searches for files like "package-env-vars.yml" or "*-env-vars.yml"
#'   }
#'
#' @return Character string with the full path to the found YAML file.
#' @examples
#' \dontrun{
#' # Find the environment variables YAML file for a package
#' yaml_path <- get_env_vars_yaml("mypackage")
#' 
#' # Read and process the YAML file
#' if (file.exists(yaml_path)) {
#'   yaml_content <- yaml::read_yaml(yaml_path)
#'   print(yaml_content$environment_variables)
#' }
#' 
#' # Using a different naming convention
#' yaml_path <- get_env_vars_yaml("mypackage", case_format = "camelCase")
#' }
#' 
#' @export
get_env_vars_yaml <- function(package, case_format = "snake_case") {
  
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
