#' qconfig Path Utilities
#'
#' Internal functions for handling path resolution, validation, and keyword processing
#' in the qconfig interactive configuration system.
#'
#' @name qconfig-path-utils
#' @keywords internal
NULL

#' Resolve Special Path Keywords
#'
#' Internal function to resolve special keywords and path combinations in paths.
#' Handles keywords like "home", "cache", "config", "data", "tempdir", "getwd", ".", ".."
#' and path combinations like "home|Documents", "cache|logs", ".|data".
#'
#' @param path_string Character string containing path with potential keywords or combinations
#' @param package Character string with package name for package-specific cache directories
#' @return Resolved path string with platform-appropriate paths
#' @keywords internal
.resolve_special_path <- function(path_string, package = NULL) {
  # Helper function to resolve keywords
  .resolve_keyword <- function(keyword) {
    switch(keyword,
      "home" = path.expand("~"),
      "cache" = tools::R_user_dir(package, "cache"),
      "config" = tools::R_user_dir(package, "config"), 
      "data" = tools::R_user_dir(package, "data"),
      "tempdir" = tempdir(),
      "getwd" = getwd(),
      "." = getwd(),           # Current directory (same as getwd)
      ".." = dirname(getwd()), # Parent directory
      keyword  # Return unchanged if not a special keyword
    )
  }
  
  # Handle path combinations (e.g., "home|Documents", "home/Documents", "home\\Documents") 
  if (grepl("[|/\\\\]", path_string)) {
    # Determine which separator is used and split accordingly
    if (grepl("\\|", path_string)) {
      parts <- strsplit(path_string, "\\|")[[1]]
    } else if (grepl("/", path_string)) {
      parts <- strsplit(path_string, "/")[[1]]
    } else if (grepl("\\\\", path_string)) {
      parts <- strsplit(path_string, "\\\\")[[1]]
    }
    
    base_path <- .resolve_keyword(parts[1])
    # Join with remaining parts
    if (length(parts) > 1) {
      return(do.call(file.path, c(list(base_path), parts[-1])))
    } else {
      return(base_path)
    }
  }
  
  # Handle single keywords
  return(.resolve_keyword(path_string))
}

#' Check if Value Contains Special Path Keywords
#'
#' Internal helper function to detect if a value contains special path keywords
#' that can be resolved dynamically at runtime.
#'
#' @param value Character string to check for keywords
#' @return Logical indicating whether the value contains special keywords
#' @keywords internal
.is_special_keyword <- function(value) {
  if (!is.character(value) || length(value) != 1) {
    return(FALSE)
  }
  
  # Direct keywords
  direct_keywords <- c("home", "cache", "config", "data", "tempdir", "getwd", ".", "..")
  if (value %in% direct_keywords) {
    return(TRUE)
  }
  
  # Path combinations with keywords (e.g., "home|Documents", "cache/logs")
  if (grepl("^(home|cache|config|data|tempdir|getwd|\\.|\\.\\.)[|/\\\\]", value)) {
    return(TRUE)
  }
  
  return(FALSE)
}

#' Process Path Input
#'
#' Internal helper function to clean and validate directory path input with interactive directory creation.
#'
#' @param path_input Character string with user-provided path input
#' @param allow_create_dir Logical; whether to allow interactive directory creation if missing
#' @return List with cleaned path, success status, and error message
#' @keywords internal
.process_path_input <- function(path_input, allow_create_dir = TRUE) {
  # Handle empty input
  if (is.null(path_input) || nchar(trimws(path_input)) == 0) {
    return(list(path = NULL, success = FALSE, message = "Path cannot be empty"))
  }
  
  # Clean path format first (without existence check)
  cleaned_path <- tryCatch({
    clean_dir_path(path_input, check_exists = FALSE, create_if_missing = FALSE)
  }, error = function(e) {
    return(NULL)
  })
  
  # Handle invalid path format
  if (is.null(cleaned_path)) {
    return(list(path = NULL, success = FALSE, message = "Invalid path format"))
  }
  
  # Check if directory exists
  if (!dir.exists(cleaned_path)) {
    if (allow_create_dir) {
      # Interactive prompt for directory creation
      .icy_text("")
      .icy_alert(paste0("Path does not exist: ", cleaned_path))
      .icy_text("Do you want to create it?")
      .icy_text("  1: Yes, create directory")
      .icy_text("  0: No, try different path (or Enter)")
      .icy_text("")
      
      repeat {
        .icy_text("Enter your choice: (0-1)", indentation = FALSE)
        user_choice <- readline()
        
        # Handle empty input (default to 0/no)
        if (nchar(trimws(user_choice)) == 0) {
          return(list(path = NULL, success = FALSE, message = ""))
        }
        
        # Parse choice
        choice <- suppressWarnings(as.integer(user_choice))
        
        if (!is.na(choice) && choice == 1) {
          # Attempt to create directory
          dir_created <- tryCatch({
            dir.create(cleaned_path, recursive = TRUE, showWarnings = FALSE)
          }, error = function(e) {
            FALSE
          })
          
          if (dir_created) {
            .icy_success(paste0("Created directory: ", cleaned_path))
            return(list(path = cleaned_path, success = TRUE, message = NULL))
          } else {
            .icy_alert(paste0("Could not create directory: ", cleaned_path))
            return(list(path = NULL, success = FALSE, message = ""))
          }
        } else if (!is.na(choice) && choice == 0) {
          # User chose not to create
          return(list(path = NULL, success = FALSE, message = ""))
        } else {
          # Invalid selection
          .icy_alert("Invalid selection. Please enter 0 or 1.")
        }
      }
    } else {
      # No creation allowed
      return(list(path = NULL, success = FALSE, message = "Path does not exist"))
    }
  }
  
  # Path exists, return success
  return(list(path = cleaned_path, success = TRUE, message = NULL))
}