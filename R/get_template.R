#' Get Template Configuration
#'
#' Reads configuration from the template YAML file (read-only blueprint).
#' Templates define the structure, defaults, types, descriptions, and options
#' for a package's configuration. Use \code{\link{get_config}} to read the
#' local configuration (single source of truth).
#'
#' @param package Character string with the package name. Defaults to `get_package_name()`.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param fn_tmpl Character string with the name or path to the template YAML file.
#'   If NULL, uses default template for the package.
#' @param case_format Character string indicating the case format to use for
#'   searching YAML files if no specific file is provided. Options are:
#'   "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param inherit Character string specifying a section to inherit values from, or
#'   0 to explicitly disable inheritance. If NULL (default), the function checks
#'   for an "inheritances" section in the template that defines automatic inheritance
#'   relationships. When explicitly specified, this parameter overrides any
#'   defined inheritance. Use 0 to disable inheritance even if defined.
#'   Inheritance is recursive - if A inherits from B and B inherits from C,
#'   A will receive values from both B and C.
#' @param verbose Logical. If TRUE, displays informative messages. Defaults to FALSE.
#' @param validate Logical. If TRUE (default), validates the template file structure.
#'   Set to FALSE to skip validation for performance.
#' @param confirm_fuzzy Logical. If TRUE (default), prompts for confirmation when fuzzy
#'   matches are used in file searching. Set to FALSE for non-interactive use.
#'
#' @return Named list of configuration values from the template.
#'
#' @examples
#' \dontrun{
#' # Get template defaults
#' template_config <- get_template(package = "mypackage")
#'
#' # Get production section from template
#' prod_defaults <- get_template(package = "mypackage", section = "production")
#'
#' # Get child section with inheritance
#' child_config <- get_template(package = "mypackage", section = "child",
#'                              inherit = "default")
#' }
#'
#' @seealso \code{\link{get_config}} for reading local configuration.
#'
#' @export
get_template <- function(package = get_package_name(),
                         section = "default",
                         fn_tmpl = NULL,
                         case_format = "snake_case",
                         inherit = NULL,
                         verbose = FALSE,
                         validate = TRUE,
                         confirm_fuzzy = TRUE) {

  # Resolve template file path
  resolved_files <- .find_config_files(
    package = package,
    fn_tmpl = fn_tmpl,
    fuzzy = TRUE,
    confirm_fuzzy = confirm_fuzzy,
    case_format = case_format,
    verbose = verbose
  )
  resolved_template_path <- resolved_files$fn_tmpl

  # Read template YAML once
  raw_template_data <- if (!is.null(resolved_template_path) && file.exists(resolved_template_path)) {
    yaml::read_yaml(resolved_template_path)
  }

  # Extract template types once
  template_types <- if (!is.null(raw_template_data) && "types" %in% names(raw_template_data)) {
    raw_template_data$types
  }

  # Validate template file (using pre-parsed data)
  if (validate && !is.null(resolved_template_path) && !is.null(raw_template_data)) {
    validation <- validate_config_file(
      fn_tmpl = resolved_template_path,
      type = "template",
      package = package,
      verbose = FALSE,
      .config_data = raw_template_data
    )

    if (!validation$valid && length(validation$errors) > 0) {
      .icy_stop(c(
        "Template validation failed",
        "x" = validation$errors[1],
        "i" = "Use validate = FALSE to skip validation"
      ))
    }
  }

  # Read template config (using pre-parsed data)
  config <- .read_yaml_config(
    package = package,
    type = "template",
    section = section,
    resolved_path = resolved_template_path,
    case_format = case_format,
    verbose = verbose,
    config_data = raw_template_data,
    template_types = template_types
  )

  # Apply inheritance
  config <- .apply_section_inheritance(
    config = config,
    raw_data = raw_template_data,
    section = section,
    inherit = inherit,
    type = "template",
    resolved_path = resolved_template_path,
    template_types = template_types,
    package = package,
    verbose = verbose
  )

  return(config)
}
