#' Find Local Configuration File
#'
#' Locates the local YAML configuration file that contains environment variable definitions
#' for a specified package. The function searches within the package's directory for a
#' local configuration file matching the expected naming pattern.
#'
#' This is a read-only operation that simply finds existing files. To create a new local
#' configuration file, use \code{\link{create_local}}.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param fn_local Character string with custom filename pattern. If NULL,
#'   uses the default naming pattern based on case_format.
#' @param case_format Character string indicating the case format to use for the filename search.
#'   Options are:
#'   \itemize{
#'     \item "snake_case" (default): Searches for files like "package_config_local.yml"
#'     \item "camelCase": Searches for files like "packageConfigLocal.yml"
#'     \item "PascalCase": Searches for files like "PackageConfigLocal.yml"
#'     \item "kebab-case": Searches for files like "package-config-local.yml"
#'   }
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to TRUE.
#'
#' @return Character string with the full path to the found YAML file, or NULL if
#'   no file is found.
#'
#' @examples
#' \dontrun{
#' # Find the local config file for a package
#' local_path <- find_local("mypackage")
#'
#' # Find with custom naming pattern
#' local_path <- find_local("mypackage", fn_local = "my_custom_config.yml")
#'
#' # Find with different case format
#' local_path <- find_local("mypackage", case_format = "camelCase")
#' }
#'
#' @export
find_local <- function(package = get_package_name(),
                       fn_local = NULL,
                       case_format = "snake_case",
                       verbose = FALSE) {
    
    # Get the filename pattern
    if (is.null(fn_local)) {
        fn_local_pattern <- .pattern(
            package = package,
            case_format = case_format,
            file = "local"
        )
    } else {
        fn_local_pattern <- fn_local
    }
    
    if (verbose) {
        fun <- sys.call()[1]
        cli::cli_text("From {.strong {fun}}: package = {.path {package}}")
        cli::cli_text("From {.strong {fun}}: fn_local_pattern = {.path {fn_local_pattern}}")
    }
    
    # Find all matching files
    matching_files <- .find_matching_pattern(
        package = package,
        fn_pattern = fn_local_pattern,
        user_dir = TRUE,
        verbose = verbose
    )
    
    if (verbose) {
        cli::cli_text("From {.strong {fun}}: length(matching_files) = {length(matching_files)}")
    }
    
    # Return results
    if (length(matching_files) == 0) {
        return(NULL)
    } else if (length(matching_files) == 1) {
        return(matching_files)
    } else {
        # Multiple files found - return first but warn if verbose
        if (verbose) {
            cli::cli_alert_warning("Multiple files found, returning first: {.file {matching_files[1]}}")
        }
        return(matching_files[1])
    }
}
