#' Get Path to User's .Renviron File
#'
#' Determines the correct path to the user's .Renviron file based on the operating system.
#' This function provides a cross-platform way to locate the standard .Renviron file where
#' R stores environment variables that are loaded at startup.
#'
#' The function handles the path differences between Windows and Unix-like systems:
#' 
#' * On Windows: Uses the USERPROFILE environment variable (typically C:\\Users\\username)
#' * On Unix/Linux/macOS: Uses the HOME environment variable (/home/username or /Users/username)
#'
#' This function is particularly useful for tools that need to read from or write to the
#' .Renviron file without hard-coding platform-specific paths.
#'
#' @return Character string with the full path to the user's .Renviron file.
#'
#' @examples
#' \dontrun{
#' # Get the path to the .Renviron file
#' renviron_path <- get_renviron_path()
#' cat("Your .Renviron file is located at:", renviron_path, "\n")
#' 
#' # Check if the file exists
#' if (file.exists(renviron_path)) {
#'   cat("The file exists.\n")
#' } else {
#'   cat("The file does not exist yet. It will be created when needed.\n")
#' }
#' 
#' # Use in functions that need to access the .Renviron file
#' backup_renviron <- function() {
#'   renviron <- get_renviron_path()
#'   if (file.exists(renviron)) {
#'     backup_file <- paste0(renviron, ".backup")
#'     file.copy(renviron, backup_file)
#'     cat("Backup created at:", backup_file, "\n")
#'   }
#' }
#' }
#'
#' @export
get_renviron_path <- function() {
  if (.Platform$OS.type == "windows") {
    path <- file.path(Sys.getenv("USERPROFILE"), ".Renviron")
  } else {
    path <- file.path(Sys.getenv("HOME"), ".Renviron")
  }
  return(path)
}
