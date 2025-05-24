#' Get Environment Variable Values
#'
#' Retrieves the values of specified environment variables from the local
#' configuration file.
#'
#' @param package Character string with the package name. If NULL (default),
#'   uses the current package name.
#' @param var_names Character vector of environment variable names to retrieve.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param fn_local Character string with custom filename for the local config.
#'   If NULL, uses the default naming pattern.
#' @param case_format Character string indicating the case format to use for filenames.
#'   Options are: "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#'
#' @return Named vector of environment variable values. Returns NA for variables
#'   not found in the configuration.
#'
#' @examples
#' \dontrun{
#' # Get specific variable values
#' values <- get_values("mypackage", c("API_KEY", "DB_HOST"))
#'
#' # Get values for current package
#' values <- get_values(var_names = c("API_KEY", "DB_HOST"))
#' }
#'
#' @export
get_values <- function(package = NULL,
                       var_names,
                       user = "default",
                       fn_local = NULL,
                       case_format = "snake_case") {
    
    # Use current package name if not provided
    if (is.null(package)) {
        package <- get_package_name()
    }
    
    # Find the local config file
    local_path <- find_local(
        package = package,
        fn_local = fn_local,
        case_format = case_format,
        verbose = TRUE
    )
    
    if (is.null(local_path)) {
        cli::cli_abort(c(
            "No local configuration file found for package {.pkg {package}}",
            "i" = "Use {.fun create_local} to create one from the template."
        ))
    }
    
    # Read the local config
    local_config <- yaml::read_yaml(local_path)
    
    # Check if user section exists
    if (!user %in% names(local_config)) {
        cli::cli_abort(c(
            "User section {.val {user}} not found in configuration",
            "i" = "Available sections: {.val {names(local_config)}}"
        ))
    }
    
    # Extract values for the requested variables
    user_config <- local_config[[user]]
    values <- user_config[var_names]
    
    # Convert to character vector and preserve names
    result <- unlist(values)
    
    # Add NA for missing variables
    missing_vars <- setdiff(var_names, names(result))
    if (length(missing_vars) > 0) {
        missing_values <- rep(NA_character_, length(missing_vars))
        names(missing_values) <- missing_vars
        result <- c(result, missing_values)
    }
    
    # Return in the order requested
    return(result[var_names])
}