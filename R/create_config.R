#' Create Configuration File
#'
#' Creates a new YAML configuration file by copying the relevant section from the
#' package's template configuration file. This function extracts the specified section
#' (default: "default") from the template and creates a new config file.
#'
#' The configuration file is where users can customize their settings, while the
#' template serves as a read-only blueprint defining the structure and default values.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param name Optional character string for named configs (e.g., "gams_switches").
#'   If NULL (default), creates the main config file (\{package\}_config.yml).
#' @param tmpl_section Character string specifying which component/section from
#'   the template to copy to the config. If NULL (default), copies only
#'   the "default" section.
#' @param overwrite Logical indicating whether to overwrite existing config file
#'   (default: FALSE).
#' @param header Character string or vector specifying the header for the config file.
#'   Options: "config" (default, uses config header), "none" (no header),
#'   or custom character vector for custom header.
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
#' @param debug Logical. If TRUE, displays detailed path information. Defaults to FALSE.
#'
#' @return Character string with the full path to the created config file.
#'
#' @examples
#' \dontrun{
#' # Create config from template's default section
#' config_path <- create_config("mypackage")
#'
#' # Create a named config
#' config_path <- create_config("mypackage", name = "gams_switches")
#'
#' # Create config from a specific template section
#' config_path <- create_config("mypackage", tmpl_section = "production")
#' }
#'
#' @export
create_config <- function(package = get_package_name(verbose = FALSE),
                          name = NULL,
                          tmpl_section = "default",
                          overwrite = FALSE,
                          header = "config",
                          verbose = FALSE,
                          debug = FALSE) {

  if (!is.character(tmpl_section)) {
    .icy_warn("tmpl_section must be a character string. Using 'default' instead.")
    tmpl_section <- "default"
  }

  # Use .find_config_files for file resolution
  files <- .find_config_files(
    package = package,
    name = name,
    fuzzy = FALSE,
    verbose = FALSE
  )

  existing <- files$fn_config
  tmpl_path <- files$fn_tmpl

  # Check if config already exists
  if (!is.null(existing) && !overwrite) {
    if (verbose) {
      .icy_warn(c(paste0("Config YAML file already exists: ", existing),
        "i" = "Use overwrite = TRUE to overwrite."
      ))
    }
    return(invisible(existing))
  } else if (verbose && !is.null(existing) && overwrite) {
    .icy_warn(paste0("Overwriting existing config YAML file: ", existing))
  }

  # Check if template was found
  if (is.null(tmpl_path)) {
    .icy_stop(paste0("No template config found for package ", package))
  }
  tmpl_config <- yaml::read_yaml(tmpl_path)

  # Extract the relevant section for config
  if (!tmpl_section %in% names(tmpl_config)) {
    .icy_stop(c(
      paste0("Component ", tmpl_section, " not found in template"),
      "i" = paste0("Available components: ", paste(names(tmpl_config), collapse = ", "))
    ))
  }

  # Determine the full path for config file
  config_filename <- .config_filename(package, name)
  config_dir <- .get_config_dir(package = package, type = "config")
  if (!dir.exists(config_dir)) {
    success <- dir.create(config_dir, recursive = TRUE)
    if (success && verbose) {
      .icy_success(paste0("Created directory: ", config_dir))
    } else if (!success) {
      .icy_warn(paste0("Failed to create directory: ", config_dir))
    }
  }
  config_path <- file.path(config_dir, config_filename)

  if (debug) {
    fun <- as.character(sys.call())
    .icy_text(paste0("From ", fun, ":"))
    .icy_text(paste0(" - config_filename = ", config_filename))
    .icy_text(paste0(" - config_dir = ", config_dir))
    .icy_text(paste0(" - config_path = ", config_path))
  }

  # Generate header using unified function with template source
  custom_header <- .generate_header(package, type = header, template_source = tmpl_path)

  # Extract data sections and inheritances from template (exclude other metadata)
  metadata_sections <- .get_metadata_sections()
  # Keep inheritances alongside data sections -- configs need it for section inheritance
  kept_sections <- setdiff(names(tmpl_config), setdiff(metadata_sections, "inheritances"))

  # Build config from template blueprint
  config_data <- list()
  for (section_name in kept_sections) {
    value <- tmpl_config[[section_name]]
    # Convert NULL inheritances to empty list so it gets written
    if (section_name == "inheritances" && is.null(value)) {
      value <- list()
    }
    config_data[[section_name]] <- value
  }

  # Write the config file using unified icy YAML writer
  .write_config_yaml(
    var_list = config_data,
    file_path = config_path,
    package = package,
    section = NULL,  # Writing complete structure with multiple sections
    template_file = tmpl_path,
    create_if_missing = TRUE,
    custom_header = custom_header,
    append_sections = FALSE,  # Creating new file, don't append
    strict_template = FALSE,  # Keep all template variables
    verbose = FALSE  # Handle messaging in create_config
  )

  if (verbose) {
    .icy_success(paste0("Created config file: ", config_path))
  }

  return(invisible(config_path))
}
