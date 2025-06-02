#' Search for YAML Files Matching Pattern in Package Directory
#'
#' Performs recursive search for YAML configuration files matching a specified
#' filename pattern within a package's directory structure. This function is the
#' core file discovery mechanism used by configuration loading functions.
#'
#' The search algorithm:
#' 1. Locates the package directory using `get_package_path()`
#' 2. Recursively finds all YAML files (*.yml, *.yaml) in the directory tree
#' 3. Filters results using case-insensitive pattern matching
#' 4. Returns full file paths for matched files
#' 5. Optionally warns about multiple matches or no matches
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param fn_pattern Character string with the filename pattern to match against YAML files.
#'   Uses regex pattern matching (case-insensitive).
#' @param user_dir Logical indicating whether to search in user directory (TRUE, default)
#'   or package installation directory (FALSE).
#' @param verbose Logical. If TRUE, displays informative messages about the search process
#'   and warnings for multiple/no matches. Defaults to FALSE.
#'
#' @return Character vector of matching file paths. Empty vector if no matches found.
#'
#' @keywords internal
.find_matching_pattern <- function(package = get_package_name(),
                                   fn_pattern,
                                   user_dir = TRUE,
                                   verbose = FALSE) {
  # Get the package path
  tryCatch(
    {
      package_dir <- get_package_path(
        package = package,
        user_dir = user_dir
      )
    },
    error = function(e) {
      cli::cli_abort("Error locating package path: {e$message}")
    }
  )
  if (verbose) cli::cli_inform("package_dir = {.val {package_dir}}")

  # Use list.files to recursively find yaml files
  yaml_files <- list.files(
    path = package_dir,
    pattern = "\\.ya?ml$",
    recursive = TRUE,
    full.names = TRUE
  )

  # Filter for files matching our pattern
  matching_files <- yaml_files[grepl(fn_pattern, yaml_files, ignore.case = TRUE)]

  if (verbose) {
    if (length(matching_files) == 0) {
      cli::cli_alert_warning("No YAML file in {.file {package_dir}} matching {.var {fn_pattern}}")
    } else if (length(matching_files) > 1) {
      cli::cli_alert_warning(
        "Multiple config YAML files found: {.file {basename(matching_files)}}. \nPlease ensure only one file is present."
      )
    }
  }

  return(as.character(matching_files))
}
