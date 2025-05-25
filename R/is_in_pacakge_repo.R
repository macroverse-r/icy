#' @keywords internal
.is_in_package_repo <- function(package = NULL, debug = FALSE) {
  current_dir <- getwd()
  desc_file <- file.path(current_dir, "DESCRIPTION")
  r_dir <- file.path(current_dir, "R")

  if (debug) {
    cli::cli_inform("basename(current_dir) = {basename(current_dir)}")
    cli::cli_inform("desc_file = {desc_file}")
    cli::cli_inform("r_dir = {r_dir}")
  }

  if (!is.null(package)) {
    cond_dir <- basename(current_dir) == package
  } else {
    cond_dir <- TRUE
  }

  is_in_package <- cond_dir && file.exists(desc_file) && dir.exists(r_dir)

  return(is_in_package)
  
}
