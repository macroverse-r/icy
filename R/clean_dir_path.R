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

#' Resolve Special Path Keywords and Template Variables
#'
#' Internal function to resolve special keywords and template variables in paths.
#' Handles both simple keywords (e.g., "documents", "cache") and template syntax
#' (e.g., "$HOME|config", "$CACHE|myapp").
#'
#' @param path_string Character string containing path with potential keywords or variables
#' @return Resolved path string with platform-appropriate paths
#' @keywords internal
.resolve_special_path <- function(path_string) {
  # First handle template variables (e.g., $HOME|config)
  if (grepl("\\$", path_string)) {
    path_string <- .resolve_template_variables(path_string)
  }
  
  # Then handle special keywords
  switch(path_string,
    "documents" = .resolve_documents_path(),
    "cache" = .resolve_cache_path(), 
    "tempdir" = tempdir(),
    "getwd" = getwd(),
    path_string  # Return processed string if not a keyword
  )
}

#' Resolve Template Variables in Path Strings
#'
#' Internal function to resolve template variables like $HOME, $CACHE, $TEMP
#' and convert | separators to platform-appropriate path separators.
#'
#' @param path_string Character string with template variables
#' @return Resolved path string
#' @keywords internal
.resolve_template_variables <- function(path_string) {
  # Replace $HOME
  if (grepl("\\$HOME", path_string)) {
    home_path <- if (.Platform$OS.type == "windows") {
      Sys.getenv("USERPROFILE") 
    } else {
      path.expand("~")
    }
    path_string <- gsub("\\$HOME", home_path, path_string)
  }
  
  # Replace $CACHE  
  if (grepl("\\$CACHE", path_string)) {
    cache_path <- if (.Platform$OS.type == "windows") {
      Sys.getenv("LOCALAPPDATA")
    } else if (Sys.info()["sysname"] == "Darwin") {
      file.path(path.expand("~"), "Library", "Caches")
    } else {
      file.path(path.expand("~"), ".cache")
    }
    path_string <- gsub("\\$CACHE", cache_path, path_string)
  }
  
  # Replace $TEMP
  if (grepl("\\$TEMP", path_string)) {
    temp_path <- if (.Platform$OS.type == "windows") {
      Sys.getenv("TEMP")
    } else {
      "/tmp"
    }
    path_string <- gsub("\\$TEMP", temp_path, path_string)
  }
  
  # Convert | to proper path separators
  path_string <- gsub("\\|", .Platform$file.sep, path_string)
  
  return(path_string)
}

#' Resolve Documents Path Cross-Platform
#'
#' Internal function to find the Documents folder across different platforms.
#'
#' @return Path to Documents folder, or home directory as fallback
#' @keywords internal
.resolve_documents_path <- function() {
  candidates <- c(
    file.path(Sys.getenv("USERPROFILE"), "Documents"),      # Windows
    file.path(Sys.getenv("USERPROFILE"), "My Documents"),   # Windows (older)
    file.path(path.expand("~"), "Documents"),               # Unix/Linux/macOS
    file.path(path.expand("~"), "My Documents")             # Unix (rare)
  )
  for (path in candidates) {
    if (dir.exists(path)) return(path)
  }
  return(path.expand("~"))  # Fallback to home
}

#' Resolve Cache Path Cross-Platform
#'
#' Internal function to get platform-appropriate cache directory.
#'
#' @return Path to cache directory
#' @keywords internal
.resolve_cache_path <- function() {
  if (.Platform$OS.type == "windows") {
    file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache")
  } else if (Sys.info()["sysname"] == "Darwin") {
    file.path(path.expand("~"), "Library", "Caches", "R") 
  } else {
    file.path(path.expand("~"), ".cache", "R")
  }
}
