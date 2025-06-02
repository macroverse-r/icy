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
#' @param package Character string with the package name.
#' @param fn_local Character string with custom filename for the local config.
#'   If NULL, uses the default naming pattern based on case_format.
#' @param fn_tmpl Character string with custom filename pattern for the template.
#'   If NULL, searches for the template using default patterns.
#' @param tmpl2local_comp Character string specifying which component/section from
#'   the template to copy to the local config. If NULL (default), copies only
#'   the "default" section.
#' @param case_format Character string indicating the case format to use for filenames.
#'   Options are: "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param force Logical indicating whether to overwrite existing local config file
#'   (default: FALSE).
#'
#' @return Character string with the full path to the created local config file.
#'
#' @examples
#' \dontrun{
#' # Create local config from template's default section
#' local_path <- create_local("mypackage")
#'
#' # Create local config from a specific template section
#' local_path <- create_local("mypackage", tmpl2local_comp = "production")
#'
#' # Create with custom filename
#' local_path <- create_local("mypackage", fn_local = "my_local_config.yml")
#' }
#'
#' @export
create_local <- function(package = get_package_name(),
                         fn_local = NULL,
                         fn_tmpl = NULL,
                         tmpl2local_comp = NULL,
                         case_format = "snake_case",
                         force = FALSE) {
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

  if (!is.null(existing) && !force) {
    cli::cli_abort(c(
      "Local config YAML file already exists: {.file {existing}}",
      "i" = "Use {.arg force = TRUE} to overwrite."
    ))
  }

  # Read template
  fn_tmpl_path <- find_template(
    package = package,
    fn_tmpl = fn_tmpl,
    case_format = case_format
  )

  if (is.null(fn_tmpl_path)) {
    cli::cli_abort("No template config ({.val {fn_tmpl_path}}) found for package {.pkg {package}}")
  }

  template_config <- yaml::read_yaml(fn_tmpl_path)

  # Extract the relevant section for local config
  if (!is.null(tmpl2local_comp)) {
    if (!tmpl2local_comp %in% names(template_config)) {
      cli::cli_abort(c(
        "Component {.val {tmpl2local_comp}} not found in template",
        "i" = "Available components: {.val {names(template_config)}}"
      ))
    }
    local_config <- template_config[[tmpl2local_comp]]
  } else {
    # Only keep the default section for the local config
    if (!"default" %in% names(template_config)) {
      cli::cli_abort(c(
        "No 'default' section found in template",
        "i" = "Available sections: {.val {names(template_config)}}"
      ))
    }
    local_config <- list()
    local_config$default <- template_config[["default"]]
  }

  # Determine the full path for local config file
  if (!grepl("[/\\\\]", fn_local)) {
    # If fn_local is just a filename, place it in the package directory
    fn_local_path <- file.path(get_package_path(package = package), fn_local)
  } else {
    # If fn_local contains path separators, use it as-is
    fn_local_path <- fn_local
  }

  if (.is_pkg_dir(package = package)) {
    fn_local_path <- file.path(fn_tmpl_path, "inst")
  }

  if (.debug()) {
    cli::cli_text("From {.strong {sys.call()[1]}}:")
    cli::cli_inform(" - fn_local = {fn_local}")
    cli::cli_inform(" - package_dir = {get_package_path(package = package)}")
    cli::cli_inform(" - fn_local_path = {fn_local_path}")
  }

  # Create the directory if it doesn't exist
  local_dir <- dirname(fn_local_path)
  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Write the local config file
  yaml::write_yaml(local_config, fn_local_path)

  if (.verbose()) {
    cli::cli_alert_success("Created local config file: {.file {fn_local_path}}")
  }

  return(fn_local_path)
}
