#' Find Template Configuration File
#'
#' Locates the template YAML configuration file that contains the blueprint for
#' environment variable definitions for a specified package. The function searches
#' within the package's installation directory (not user directory) for the template
#' configuration file.
#'
#' Template files are read-only blueprints that define the structure and default
#' values for environment variables. They should be located in the package's
#' installation directory, typically in the inst/ folder.
#'
#' @param package Character string with the package name. If NULL (default),
#'   uses the current package name.
#' @param fn_tmpl Character string with custom filename pattern for the template.
#'   If NULL, uses the default naming pattern based on case_format.
#' @param case_format Character string indicating the case format to use for the filename search.
#'   Options are:
#'   \itemize{
#'     \item "snake_case" (default): Searches for files like "package_config_template.yml"
#'     \item "camelCase": Searches for files like "packageConfigTemplate.yml"
#'     \item "PascalCase": Searches for files like "PackageConfigTemplate.yml"
#'     \item "kebab-case": Searches for files like "package-config-template.yml"
#'   }
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
#'
#' @return Character string with the full path to the found template file, or NULL
#'   if no template is found.
#'
#' @examples
#' \dontrun{
#' # Find the template config file for a package
#' template_path <- find_template("mypackage")
#'
#' # Find with custom naming pattern
#' template_path <- find_template("mypackage", fn_tmpl = "my_template.yml")
#'
#' # Find with different case format
#' template_path <- find_template("mypackage", case_format = "camelCase")
#' }
#'
#' @export
find_template <- function(package = get_package_name(),
                          fn_tmpl = NULL,
                          case_format = "snake_case",
                          verbose = FALSE) {

  if (!is.null(fn_tmpl)) {
    # If custom template filename is provided
    if (!grepl("[/\\\\]", fn_tmpl)) {
      # Just a filename, search for it
      tmpl_path <- .find_matching_pattern(
        package = package,
        fn_pattern = fn_tmpl,
        user_dir = FALSE,
        verbose = verbose
      )
    } else {
      # Full path provided, check if it exists
      if (file.exists(fn_tmpl)) {
        tmpl_path <- fn_tmpl
      } else {
        if (verbose) {
          cli::cli_alert_warning("Template file not found: {.file {fn_tmpl}}")
        }
        tmpl_path <- character(0)
      }
    }
  } else {
    # Use default pattern
    tmpl_pattern <- .pattern(
      package = package,
      case_format = case_format,
      file = "template"
    )
    if (verbose) {
      cli::cli_inform("tmpl_pattern = {.val {tmpl_pattern}}")
    }
    tmpl_path <- .find_matching_pattern(
      package = package,
      fn_pattern = tmpl_pattern,
      user_dir = FALSE,
      verbose = verbose
    )
  }
  if (verbose) {
    cli::cli_inform("tmpl_path = {.val {tmpl_path}}")
  }

  # Return results
  if (length(tmpl_path) == 0) {
    return(NULL)
  } else if (length(tmpl_path) == 1) {
    return(tmpl_path)
  } else {
    # Multiple templates found - this is likely an error
    if (verbose) {
      cli::cli_alert_warning(c(
        "Multiple template files found:",
        "{.file {basename(tmpl_path)}}",
        "Returning first: {.file {tmpl_path[1]}}"
      ))
    }
    return(tmpl_path[1])
  }
}

