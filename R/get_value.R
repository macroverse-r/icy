#' Get a Single Configuration Value
#'
#' Retrieves a single configuration value by name. This function is a convenience
#' wrapper around `get_config()` that returns the value directly instead of a list.
#'
#' @param name Character string with the variable name to retrieve.
#' @param package Character string with the package name. Defaults to `get_package_name()`.
#' @param origin Character string specifying where to read from. Defaults to "priority".
#' @param section Character string for the section in the YAML file. Defaults to "default".
#' @param yaml_file Character string with the name or path to the YAML file. Defaults to NULL.
#' @param case_format Character string indicating the case format. Defaults to "snake_case".
#'
#' @return The value of the specified configuration variable, or NULL if not found.
#'
#' @examples
#' \dontrun{
#' # Get a specific configuration value
#' api_key <- get_value("DUMMY_API_KEY")
#' 
#' # Get from a specific origin
#' debug_mode <- get_value("DUMMY_DEBUG", origin = "local")
#' }
#'
#' @export
get_value <- function(name,
                      package = get_package_name(),
                      origin = "priority",
                      section = "default",
                      yaml_file = NULL,
                      case_format = "snake_case") {

  config <- get_config(package = package,
                       origin = origin,
                       section = section,
                       yaml_file = yaml_file,
                       case_format = case_format)
  
  # Check if variable exists in config
  if (!name %in% names(config)) {
    .icy_stop(c(
      paste0("Variable '", name, "' not found in ", origin, " configuration for package '", package, "'"),
      "i" = paste0("Available variables: ", paste(names(config), collapse = ", "))
    ))
  }
  
  raw_value <- config[[name]]
  
  # Handle dynamic keyword resolution for path types
  # If the value is a special keyword, resolve it at runtime
  if (is.character(raw_value) && length(raw_value) == 1 && .is_special_keyword(raw_value)) {
    tryCatch({
      resolved_value <- .resolve_special_path(raw_value, package)
      # Clean the resolved path
      clean_path <- clean_dir_path(resolved_value, check_exists = FALSE, create_if_missing = FALSE)
      return(clean_path)
    }, error = function(e) {
      .icy_warn(paste0("Could not resolve dynamic keyword '", raw_value, "': ", e$message))
      return(raw_value)  # Return original value if resolution fails
    })
  }
  
  return(raw_value)
}
