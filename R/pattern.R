
#' Generate YAML File Search Patterns Based on Case Format
#'
#' Creates filename patterns for YAML configuration files based on package name,
#' case format, and file type. This function standardizes filename generation
#' across different naming conventions to support flexible file discovery.
#'
#' The function supports four case formats:
#' - `snake_case`: "package_config_local.yml" (default)
#' - `camelCase`: "packageConfigLocal.yml" 
#' - `PascalCase`: "PackageConfigLocal.yml"
#' - `kebab-case`: "package-config-local.yml"
#'
#' Pattern structure: `{package}{prefix}config{separator}{file}{extension}`
#' where prefix, separator, and transform rules depend on the case format.
#'
#' @param package Character string with the package name used as filename prefix. Defaults to `get_package_name()` to detect the calling package.
#' @param case_format Character string specifying the case format. Options are:
#'   "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param file Character string specifying the file type suffix (e.g., "local", "template").
#'   Defaults to "local".
#' @param yml Logical. If TRUE, uses exact ".yml" extension. If FALSE (default),
#'   uses regex pattern "\\.ya?ml$" to match both .yml and .yaml extensions.
#'
#' @return Character string containing the generated filename pattern.
#'
#' @keywords internal
.pattern <- function(package = get_package_name(),
                     case_format = "snake_case",
                     file = "local",
                     yml = FALSE) {
  
  # Define case format rules
  case_rules <- list(
    "snake_case" = list(prefix = "_", 
                        separator = "_", 
                        transform = tolower),

    "camelCase" = list(prefix = "", 
                       separator = "", 
                       transform = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))),

    "PascalCase" = list(prefix = "", 
                        separator = "", 
                        transform = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))),

    "kebab-case" = list(prefix = "-", 
                        separator = "-", 
                        transform = tolower)
  )
  
  # Get the appropriate rule or default to snake_case
  rule <- case_rules[[case_format]] %||% case_rules[["snake_case"]]
  
  # Build the pattern
  config_part <- rule$transform("config")
  file_part_transformed <- rule$transform(file)
  
  if (yml) {
    extension <- ".yml"
  } else {
    extension <- "\\.ya?ml$"
  }

  pattern <- paste0(
    package,
    rule$prefix,
    config_part,
    rule$separator,
    file_part_transformed,
    extension
  )
  
  return(pattern)
}
