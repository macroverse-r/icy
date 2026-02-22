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