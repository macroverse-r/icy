#' Create Local Configuration File
#'
#' Creates a new local YAML configuration file for environment variables by copying
#' the relevant section from the package's template configuration file. This function
#' extracts the specified section (default: "default") from the template and creates
#' a new local configuration file.
#'
#' The local configuration file is where users can customize their environment variable
#' values, while the template serves as a read-only blueprint defining the structure
#' and default values.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param fn_local Character string with custom filename for the local config.
#'   If NULL, uses the default naming pattern based on case_format.
#' @param fn_tmpl Character string with custom filename pattern for the template.
#'   If NULL, searches for the template using default patterns.
#' @param tmpl_section Character string specifying which component/section from
#'   the template to copy to the local config. If NULL (default), copies only
#'   the "default" section.
#' @param overwrite Logical indicating whether to overwrite existing local config file
#'   (default: FALSE).
#' @param case_format Character string indicating the case format to use for filenames.
#'   Options are: "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
#'
#' @return Character string with the full path to the created local config file.
#'
#' @examples
#' \dontrun{
#' # Create local config from template's default section
#' local_path <- create_local("mypackage")
#'
#' # Create local config from a specific template section
#' local_path <- create_local("mypackage", tmpl_section = "production")
#'
#' # Create with custom filename
#' local_path <- create_local("mypackage", fn_local = "my_local_config.yml")
#' }
#'
#' @export
create_local <- function(package = get_package_name(),
                         fn_local = NULL,
                         fn_tmpl = NULL,
                         tmpl_section = "default",
                         overwrite = FALSE,
                         case_format = "snake_case",
                         verbose = FALSE) {

  if (!is.character(tmpl_section)) {
    icy_warn("tmpl_section must be a character string. Using 'default' instead.")
    tmpl_section <- "default"
  }

  # Use default naming convention if fn_local is not specified
  if (is.null(fn_local)) {
    fn_local <- .pattern(
      package = package,
      case_format = case_format,
      file = "local",
      yml = TRUE
    )
  }

  # Check if local config already exists
  existing <- find_local(
    package = package,
    fn_local = fn_local,
    case_format = case_format,
    verbose = FALSE
  )

  if (!is.null(existing) && !overwrite) {
    if (verbose) {
      icy_warn(c("Local config YAML file already exists: (existing)",
        "i" = "Use {.arg overwrite = TRUE} to overwrite."
      ))
    }
    return(invisible(existing))
  } else if (verbose && !is.null(existing) && overwrite) {
    icy_alert_danger("Overwriting existing local config YAML file: (existing)")
  }

  # Read template
  tmpl_path <- find_template(
    package = package,
    fn_tmpl = fn_tmpl,
    case_format = case_format
  )

  if (is.null(tmpl_path)) {
    icy_abort("No template config ((tmpl_path)) found for package {.pkg {package)")
  }
  tmpl_config <- yaml::read_yaml(tmpl_path)

  # Extract the relevant section for local config
  if (!tmpl_section %in% names(tmpl_config)) {
    icy_abort(c(
      "Component (tmpl_section) not found in template",
      "i" = "Available components: (names(tmpl_config))"
    ))
  }
  local_config <- list()
  local_config[[tmpl_section]] <- tmpl_config[[tmpl_section]]

  # Determine the full path for local config file
  # If fn_local is just a filename, place it in the package directory
  if (!grepl("[/\\\\]", fn_local)) {
    # If in package directory: local location = template location
    if (.is_pkg_dir(package = package)) {
      local_path <- file.path(dirname(tmpl_path), fn_local)
    } else {
      local_path <- file.path(get_package_path(package = package), fn_local)
    }
  } else {
    # If fn_local contains path separators, use it as-is (user is responsible)
    local_path <- fn_local
  }

  if (verbose) {
    fun <- as.character(sys.call())
    icy_text("From {.strong {fun):")
    icy_inform(" - fn_local = {fn_local}")
    icy_inform(" - package_dir = {get_package_path(package = package)}")
    icy_inform(" - local_path = {local_path}")
  }

  # Create the directory if it doesn't exist
  local_dir <- dirname(local_path)
  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Write the local config file
  yaml::write_yaml(local_config, local_path)

  if (verbose) {
    icy_alert_success("Created local config file: (local_path)")
  }

  return(local_path)
}
