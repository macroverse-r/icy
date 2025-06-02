#' @keywords internal
.is_pkg_dir <- function(package = get_package_name(),
                        debug = FALSE) {
  current_dir <- getwd()

  has_r_dir <- dir.exists("R")
  has_description <- file.exists("DESCRIPTION")
  has_namespace <- file.exists("NAMESPACE")

  if (debug) {
    cli::cli_inform("basename(current_dir) = {basename(current_dir)}")
  }

  if (is.null(package) && has_r_dir && has_description) {
    desc <- readLines("DESCRIPTION", warn = FALSE)
    package_line <- desc[grep("^Package:", desc)]
    if (length(package_line) > 0) {
      package <- trimws(sub("^Package:", "", package_line))
    }

  }
  
  has_pkg_dir_name <- basename(current_dir) == package

  is_in_pkg_dir <- has_pkg_dir_name && has_description && has_namespace && has_r_dir

  return(is_in_pkg_dir)
  
}
