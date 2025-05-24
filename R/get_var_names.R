#' Get Environment Variable Names
#'
#' Retrieves the list of environment variable names defined in a package's
#' configuration file. This function is useful for discovering what environment
#' variables a package expects to use.
#'
#' @param package Character string with the package name. If NULL (default),
#'   uses the current package name.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param origin Character string specifying where to read the configuration from:
#'   - "template": Read from the package's template YAML file
#'   - "local": Read from the user's local configuration file (default)
#'   - "renviron": Read from .Renviron file
#'   - "priority": Read with priority order (.Renviron > local config)
#' @param yaml_file Character string with custom filename. If NULL,
#'   uses the default naming pattern.
#' @param case_format Character string indicating the case format to use for filenames.
#'   Options are: "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#'
#' @return Character vector of environment variable names.
#'
#' @examples
#' \dontrun{
#' # Get variable names from local config
#' var_names <- get_var_names("mypackage")
#'
#' # Get variable names from template (to see all possible variables)
#' all_vars <- get_var_names("mypackage", origin = "template")
#'
#' # Use these names for validation or to retrieve current values
#' current_values <- lapply(var_names, Sys.getenv, unset = NA)
#' names(current_values) <- var_names
#' }
#'
#' @export
get_var_names <- function(package = NULL,
                          user = "default",
                          origin = "local",
                          yaml_file = NULL,
                          case_format = "snake_case") {
    
    # Get the configuration
    config <- get_config(
        package = package,
        user = user,
        origin = origin,
        yaml_file = yaml_file,
        case_format = case_format
    )
    
    # Return the names of the configuration entries
    return(names(config))
}