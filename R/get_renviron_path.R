#' Get Path to User's .Renviron File
#'
#' Determines the correct path to the user's .Renviron file based on the operating system.
#'
#' @return Character string with the full path to the .Renviron file.
#'
#' @export
get_renviron_path <- function() {
  if (.Platform$OS.type == "windows") {
    file.path(Sys.getenv("USERPROFILE"), ".Renviron")
  } else {
    file.path(Sys.getenv("HOME"), ".Renviron")
  }
}