#' Clean and Normalize a Directory Path
#'
#' Standardizes directory paths across platforms and optionally verifies or creates
#' directories. This utility function helps ensure consistent handling of file paths
#' across different operating systems and environments.
#'
#' The function performs the following operations:
#' 
#' * Removes trailing slashes
#' * Normalizes path separators (converts backslashes to forward slashes on Windows)
#' * Optionally checks if the directory exists
#' * Optionally creates the directory if it does not exist
#'
#' This function is particularly useful when processing user-provided paths or when
#' writing cross-platform code that needs to handle paths consistently.
#'
#' @param path Character string with the directory path to clean and normalize.
#' @param check_exists Logical; whether to check if the directory exists.
#'   Default is TRUE.
#' @param create_if_missing Logical; whether to create the directory if it
#'   doesn't exist and `check_exists` is TRUE. Default is FALSE.
#'
#' @return A normalized path string, or NULL if the path is invalid
#'   (e.g., if check_exists is TRUE and the directory doesn't exist).
#'
#' @examples
#' \dontrun{
#' # Basic path normalization
#' clean_dir_path("/path/to/directory/")
#' # Returns: "/path/to/directory"
#' 
#' # Path with Windows-style separators
#' clean_dir_path("C:\\Users\\username\\Documents\\")
#' # Returns: "C:/Users/username/Documents"
#' 
#' # Check if directory exists without creating it
#' clean_dir_path("/path/that/might/not/exist", check_exists = TRUE)
#' # Returns: NULL if the directory doesn't exist
#' 
#' # Clean path and create directory if it doesn't exist
#' clean_dir_path("/path/to/new/directory", 
#'               check_exists = TRUE, 
#'               create_if_missing = TRUE)
#' # Creates the directory if needed and returns the normalized path
#' 
#' # Using in a configuration function
#' save_user_preferences <- function(data_dir) {
#'   # Clean and validate the path, creating it if needed
#'   clean_dir <- clean_dir_path(data_dir, 
#'                              check_exists = TRUE, 
#'                              create_if_missing = TRUE)
#'   if (is.null(clean_dir)) {
#'     stop("Invalid directory path")
#'   }
#'   
#'   # Now use the clean path for file operations
#'   config_file <- file.path(clean_dir, "preferences.json")
#'   # ... save preferences to file
#' }
#' }
#'
#' @export
clean_dir_path <- function(path,
                            check_exists = TRUE,
                            create_if_missing = FALSE) {
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
        .icy_warn(paste0("Could not create directory: ", clean_path))
        return(NULL)
      }
      .icy_success(paste0("Created directory: ", clean_path))
    } else {
      .icy_warn(paste0("Directory does not exist: ", clean_path))
      return(NULL)
    }
  }
  
  return(clean_path)
}
