#' Ensure .Rbuildignore Excludes Local Config Directory
#'
#' Helper function to automatically add the inst/local_config/ directory to .Rbuildignore
#' when creating local configuration files in package development directories. This prevents
#' local development configs from being included in package builds.
#'
#' @param package_root Character string with the package root directory. Defaults to current working directory.
#' @param verbose Logical. If TRUE, shows informative messages. Defaults to FALSE.
#' @return Logical. TRUE if .Rbuildignore was modified, FALSE if pattern already existed.
#' @keywords internal
.ensure_rbuildignore_excludes_local_config <- function(package_root = getwd(), verbose = FALSE) {
  
  rbuildignore_path <- file.path(package_root, ".Rbuildignore")
  pattern <- "^inst/local_config/"
  
  # Read existing .Rbuildignore or create empty vector
  if (file.exists(rbuildignore_path)) {
    if (verbose) .icy_text(paste0("Reading existing .Rbuildignore: ", rbuildignore_path))
    lines <- readLines(rbuildignore_path, warn = FALSE)
  } else {
    if (verbose) .icy_text(paste0("Creating new .Rbuildignore: ", rbuildignore_path))
    lines <- character(0)
  }
  
  # Check if pattern already exists
  if (any(grepl(pattern, lines, fixed = TRUE))) {
    if (verbose) .icy_text("Pattern ^inst/local_config/ already in .Rbuildignore")
    return(FALSE)
  }
  
  # Add pattern to exclude local config directory
  lines <- c(lines, pattern)
  
  # Write back to file
  writeLines(lines, rbuildignore_path)
  
  if (verbose) {
    .icy_success("Added ^inst/local_config/ to .Rbuildignore")
  }
  
  return(TRUE)
}

#' Create Directory if it Doesn't Exist
#'
#' Helper function to create a directory and its parent directories if they don't exist.
#' Used for ensuring inst/local_config/ directory exists when creating local configs.
#'
#' @param dir_path Character string with the directory path to create.
#' @param verbose Logical. If TRUE, shows informative messages. Defaults to FALSE.
#' @return Logical. TRUE if directory was created, FALSE if it already existed.
#' @keywords internal
.ensure_directory_exists <- function(dir_path, verbose = FALSE) {
  
  if (dir.exists(dir_path)) {
    if (verbose) .icy_text(paste0("Directory already exists: ", dir_path))
    return(FALSE)
  }
  
  # Create directory and parents
  success <- dir.create(dir_path, recursive = TRUE)
  
  if (success) {
    if (verbose) .icy_success(paste0("Created directory: ", dir_path))
  } else {
    .icy_warn(paste0("Failed to create directory: ", dir_path))
  }
  
  return(success)
}