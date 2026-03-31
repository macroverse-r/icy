#' Get Configuration
#'
#' Reads configuration from the local YAML file (single source of truth).
#' The local config file is the authoritative source for all configuration values.
#' Use \code{\link{get_template}} to read the template file (blueprint).
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param fn_tmpl Character string with the name or path to the template YAML file.
#'   If NULL, uses default template for the package. Used for validation and type resolution.
#' @param fn_local Character string with the name or path to the local YAML file.
#'   If NULL, uses default local config for the package.
#' @param case_format Character string indicating the case format to use for
#'   searching YAML files if no specific files are provided. Options are:
#'   "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param inherit Character string specifying a section to inherit values from, or
#'   0 to explicitly disable inheritance. If NULL (default), the function checks
#'   for an "inheritances" section in the config that defines automatic inheritance
#'   relationships. When explicitly specified, this parameter overrides any
#'   defined inheritance. Use 0 to disable inheritance even if defined.
#'   Values from the inherit section are used as defaults, which can
#'   be overridden by the main section. Inheritance is recursive - if A inherits
#'   from B and B inherits from C, A will receive values from both B and C.
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
#' @param validate Logical. If TRUE (default), validates the configuration file
#'   structure. Set to FALSE to skip validation for performance.
#' @param confirm_fuzzy Logical. If TRUE (default), prompts for confirmation when fuzzy
#'   matches are used in file searching. Set to FALSE for non-interactive use.
#'
#' @return Named list of configuration values.
#'
#' @examples
#' \dontrun{
#' # Get configuration from local file (single source of truth)
#' config <- get_config(package = "mypackage")
#'
#' # Get production config with defaults inherited from default section
#' prod_config <- get_config(package = "mypackage", section = "production",
#'                          inherit = "default")
#' }
#'
#' @seealso \code{\link{get_template}} for reading template configuration.
#'
#' @export
get_config <- function(package = get_package_name(),
                       section = "default",
                       fn_tmpl = NULL,
                       fn_local = NULL,
                       case_format = "snake_case",
                       inherit = NULL,
                       verbose = FALSE,
                       validate = TRUE,
                       confirm_fuzzy = TRUE) {

  # Resolve file paths
  resolved_files <- .find_config_files(
    package = package,
    fn_local = fn_local,
    fn_tmpl = fn_tmpl,
    fuzzy = TRUE,
    confirm_fuzzy = confirm_fuzzy,
    case_format = case_format,
    verbose = verbose
  )

  resolved_local_path <- resolved_files$fn_local
  resolved_template_path <- resolved_files$fn_tmpl

  # Read YAML files once
  raw_local_data <- if (!is.null(resolved_local_path) && file.exists(resolved_local_path)) {
    yaml::read_yaml(resolved_local_path)
  }

  raw_template_data <- if (!is.null(resolved_template_path) && file.exists(resolved_template_path)) {
    tryCatch(yaml::read_yaml(resolved_template_path), error = function(e) NULL)
  }

  # Extract template types once
  template_types <- if (!is.null(raw_template_data) && "types" %in% names(raw_template_data)) {
    raw_template_data$types
  }

  # Validate local configuration file (using pre-parsed data)
  if (validate && !is.null(resolved_local_path) && !is.null(raw_local_data)) {
    validation <- validate_config_file(
      fn_local = resolved_local_path,
      fn_tmpl = resolved_template_path,
      type = "local",
      package = package,
      verbose = FALSE,
      .config_data = raw_local_data,
      .template_data = raw_template_data
    )

    if (!validation$valid && length(validation$errors) > 0) {
      .icy_stop(c(
        "Local config validation failed",
        "x" = validation$errors[1],
        "i" = "Use validate = FALSE to skip validation"
      ))
    }
  }

  # Extract and process the requested section
  config <- .process_config_section(
    config_data = raw_local_data,
    section = section,
    template_types = template_types,
    package = package,
    source_label = "local config"
  )

  # Apply inheritance
  config <- .apply_section_inheritance(
    config = config,
    raw_data = raw_local_data,
    section = section,
    inherit = inherit,
    type = "local",
    template_types = template_types,
    package = package,
    verbose = verbose
  )

  # Check for conflicts
  check_conflicts(
    package = package,
    mode = "warn",
    config = config
  )

  return(config)
}
