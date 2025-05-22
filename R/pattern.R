
#' Define search patterns based on case format
#'
#' @param case_format Character string specifying the case format
#' @param file Character string specifying the file type
#'
#' @keywords internal
.pattern <- function(package,
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
