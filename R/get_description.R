#' Get Configuration Variable Descriptions
#'
#' Retrieves descriptions for configuration variables from the package's template file.
#' This function reads the 'descriptions' section of the template YAML file to provide
#' documentation about what each configuration variable does.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param var_name Character string specifying a specific variable name to get description for.
#'   If NULL (default), returns descriptions for all variables.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param yaml_file Character string with the name or path to the template YAML file. If NULL,
#'   the function will search for the appropriate template file.
#' @param case_format Character string indicating the case format to use for
#'   searching the YAML file if `yaml_file` is NULL. Options are:
#'   "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
#'
#' @return If `var_name` is specified, returns a character string with the description.
#'   If `var_name` is NULL, returns a named list of all descriptions.
#'   Returns NULL if no descriptions section is found or if the specific variable has no description.
#'
#' @examples
#' \dontrun{
#' # Get all variable descriptions
#' descriptions <- get_description(package = "mypackage")
#'
#' # Get description for a specific variable
#' api_desc <- get_description(package = "mypackage", var_name = "MY_API_KEY")
#'
#' # Check if a variable has a description
#' if (!is.null(get_description("mypackage", "MY_VAR"))) {
#'   cat("Variable is documented")
#' }
#' }
#'
#' @export
get_description <- function(package = get_package_name(),
                           var_name = NULL,
                           section = "default",
                           yaml_file = NULL,
                           case_format = "snake_case",
                           verbose = FALSE) {

  # Get the template configuration which should contain descriptions
  template_path <- tryCatch({
    find_template(
      package = package,
      fn_tmpl = yaml_file,
      case_format = case_format,
      verbose = FALSE
    )
  }, error = function(e) {
    if (verbose) {
      .icy_warn(paste0("Could not find template file for package '", package, "': ", e$message))
    }
    return(NULL)
  })

  if (is.null(template_path)) {
    if (verbose) {
      .icy_warn(paste0("No template file found for package '", package, "'"))
    }
    return(NULL)
  }

  # Read the YAML file
  yaml_content <- tryCatch({
    yaml::read_yaml(template_path)
  }, error = function(e) {
    if (verbose) {
      .icy_warn(paste0("Could not read template file '", template_path, "': ", e$message))
    }
    return(NULL)
  })

  if (is.null(yaml_content)) {
    return(NULL)
  }

  # Extract descriptions section
  descriptions <- yaml_content[["descriptions"]]
  
  if (is.null(descriptions) || length(descriptions) == 0) {
    if (verbose) {
      .icy_alert(paste0("No descriptions section found in template file for package '", package, "'"))
    }
    return(NULL)
  }

  # If specific variable requested, return its description
  if (!is.null(var_name)) {
    description <- descriptions[[var_name]]
    if (is.null(description)) {
      if (verbose) {
        .icy_warn(paste0("No description found for variable '", var_name, "'"))
      }
      return(NULL)
    }
    return(as.character(description))
  }

  # Return all descriptions
  if (verbose) {
    .icy_success(paste0("Retrieved ", length(descriptions), " variable description", 
                             if(length(descriptions) > 1) "s" else "", 
                             " from package '", package, "'"))
  }

  return(descriptions)
}
