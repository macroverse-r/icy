#' Validate and Normalize a Directory Path
#'
#' Helper function to validate a directory path and ensure it's properly formatted.
#'
#' @param path The path to validate
#' @param check_exists Whether to check if the directory exists
#' @param create_if_missing Whether to try to create the directory if it doesn't exist
#'
#' @return A normalized path or NULL if invalid
#' @export
validate_directory_path <- function(path, check_exists = TRUE, create_if_missing = FALSE) {
  # Remove trailing slashes and normalize path separators
  clean_path <- sub("/*$", "", path)
  
  # On Windows, ensure consistent path separators by replacing \ with /
  if (.Platform$OS.type == "windows") {
    clean_path <- gsub("\\\\", "/", clean_path)
  }
  
  # Check if directory exists if requested
  if (check_exists && !dir.exists(clean_path)) {
    if (create_if_missing) {
      # Try to create the directory
      dir_created <- dir.create(clean_path, recursive = TRUE, showWarnings = FALSE)
      if (!dir_created) {
        cli::cli_alert_warning("Could not create directory: {.file {clean_path}}")
        return(NULL)
      }
      cli::cli_alert_success("Created directory: {.file {clean_path}}")
    } else {
      cli::cli_alert_warning("Directory does not exist: {.file {clean_path}}")
      return(NULL)
    }
  }
  
  return(clean_path)
}