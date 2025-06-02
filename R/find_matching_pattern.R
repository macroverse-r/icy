#' Find files matching a pattern within a package directory
#'
#' Internal function that searches for YAML files matching a given pattern
#' within a package's directory structure.
#'
#' @param package Character string with the package name
#' @param fn_pattern Character string with the filename pattern to match
#' @param user_dir Logical indicating whether to search in user directory (default: TRUE)
#' @param verbose Logical indicating whether to display warnings (default: FALSE)
#'
#' @return Character vector of matching file paths
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
      stop("Error locating package path: ", e$message)
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
