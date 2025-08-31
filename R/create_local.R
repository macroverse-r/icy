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
#' @param header Character string or vector specifying the header for the local config file.
#'   Options: "local" (default, uses local config header), "none" (no header), 
#'   or custom character vector for custom header.
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
#' @param debug Logical. If TRUE, displays detailed path information. Defaults to FALSE.
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
create_local <- function(package = get_package_name(verbose = FALSE),
                         fn_local = NULL,
                         fn_tmpl = NULL,
                         tmpl_section = "default",
                         overwrite = FALSE,
                         case_format = "snake_case",
                         header = "local",
                         verbose = FALSE,
                         debug = FALSE) {

  if (!is.character(tmpl_section)) {
    .icy_warn("tmpl_section must be a character string. Using 'default' instead.")
    tmpl_section <- "default"
  }

  # Use find_config_files for intelligent filename derivation and pairing
  files <- find_config_files(
    package = package,
    fn_tmpl = fn_tmpl,
    fn_local = fn_local,
    fuzzy = FALSE,    # Use exact matching for checking existing files
    case_format = case_format,
    verbose = FALSE
  )
  
  existing <- files$fn_local
  tmpl_path <- files$fn_tmpl
  
  # Check if local config already exists
  if (!is.null(existing) && !overwrite) {
    if (verbose) {
      .icy_warn(c(paste0("Local config YAML file already exists: ", existing),
        "i" = "Use overwrite = TRUE to overwrite."
      ))
    }
    return(invisible(existing))
  } else if (verbose && !is.null(existing) && overwrite) {
    .icy_warn(paste0("Overwriting existing local config YAML file: ", existing))
  }

  # Check if template was found
  if (is.null(tmpl_path)) {
    .icy_stop(paste0("No template config found for package ", package))
  }
  tmpl_config <- yaml::read_yaml(tmpl_path)

  # Extract the relevant section for local config
  if (!tmpl_section %in% names(tmpl_config)) {
    .icy_stop(c(
      paste0("Component ", tmpl_section, " not found in template"),
      "i" = paste0("Available components: ", paste(names(tmpl_config), collapse = ", "))
    ))
  }

  # Determine the full path for local config file
  # If fn_local is NULL, generate default filename
  if (is.null(fn_local)) {
    # Generate default local filename using same pattern as find_file
    fn_local <- .pattern(
      package = package,
      case_format = case_format,
      file = "local",
      yml = TRUE
    )
  }
  
  # If fn_local is just a filename, place it in the appropriate directory  
  if (!grepl("[/\\\\]", fn_local)) {
    # Get the local config directory
    local_dir <- get_config_dir(package = package, type = "local")
    
    # If in package directory, ensure directory structure exists
    if (.is_pkg_dir(package = package)) {
      # Ensure inst/local_config/ directory exists
      .ensure_directory_exists(local_dir, verbose = verbose)
      
      # Ensure .Rbuildignore excludes this directory
      .ensure_rbuildignore_excludes_local_config(verbose = verbose)
    }
    
    local_path <- file.path(local_dir, fn_local)
  } else {
    # If fn_local contains path separators, use it as-is (user is responsible)
    local_path <- fn_local
  }

  if (debug) {
    fun <- as.character(sys.call())
    .icy_text(paste0("From ", fun, ":"))
    .icy_text(paste0(" - fn_local = ", fn_local))
    .icy_text(paste0(" - local_config_dir = ", get_config_dir(package = package, type = "local")))
    .icy_text(paste0(" - local_path = ", local_path))
  }

  # Generate header using unified function with template source
  custom_header <- .generate_header(package, type = header, template_source = tmpl_path)
  
  # Extract all data sections from template (exclude metadata sections except inheritances)
  metadata_sections <- .get_metadata_sections()
  data_sections <- setdiff(names(tmpl_config), metadata_sections)
  
  # Build complete local config with all data sections
  local_config_data <- list()
  for (section_name in data_sections) {
    if (section_name %in% names(tmpl_config)) {
      local_config_data[[section_name]] <- tmpl_config[[section_name]]
    }
  }
  
  # Always include inheritances section (required for local configs)
  # Use template's inheritances if available, otherwise empty list
  if ("inheritances" %in% names(tmpl_config)) {
    # Convert NULL to empty list to ensure it gets written
    inheritances_value <- tmpl_config[["inheritances"]]
    if (is.null(inheritances_value)) {
      inheritances_value <- list()
    }
    local_config_data[["inheritances"]] <- inheritances_value
  } else {
    # Ensure inheritances section exists even if template doesn't have it
    local_config_data[["inheritances"]] <- list()
  }
  
  # Write the local config file using unified icy YAML writer
  # This provides template validation, variable ordering, and proper NULL handling
  .write_config_yaml(
    var_list = local_config_data,
    file_path = local_path,
    package = package,
    section = NULL,  # Writing complete structure with multiple sections
    template_file = tmpl_path,
    create_if_missing = TRUE,
    custom_header = custom_header,
    append_sections = FALSE,  # Creating new file, don't append
    strict_template = FALSE,  # Keep all template variables
    verbose = FALSE  # Handle messaging in create_local
  )

  if (verbose) {
    .icy_success(paste0("Created local config file: ", local_path))
  }

  return(invisible(local_path))
}
