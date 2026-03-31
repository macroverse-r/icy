#' Check if Current Directory is a Package Directory
#'
#' Validates whether the current working directory is a valid R package directory
#' by checking for required package files and matching the DESCRIPTION Package field.
#'
#' The function performs several checks:
#' 1. Presence of R/ directory (contains source code)
#' 2. Presence of DESCRIPTION file (package metadata)
#' 3. Presence of NAMESPACE file (export/import declarations)
#' 4. DESCRIPTION Package field matches the specified package name
#'
#' @param package Character string with the package name. If NULL (default),
#'   returns TRUE for any valid package directory.
#' @param debug Logical. If TRUE, displays debugging information about directory validation. Defaults to FALSE.
#'
#' @return Logical. TRUE if current directory is a valid package directory matching
#'   the specified package name, FALSE otherwise.
#'
#' @keywords internal
.is_pkg_dir <- function(package = NULL,
                        debug = FALSE) {
  has_r_dir <- dir.exists("R")
  has_description <- file.exists("DESCRIPTION")
  has_namespace <- file.exists("NAMESPACE")

  if (!has_r_dir || !has_description || !has_namespace) {
    return(FALSE)
  }

  # Match by DESCRIPTION Package field, not directory name
  desc_pkg <- .get_package_name_from_description()

  if (debug) {
    .icy_text(paste0("DESCRIPTION Package: ", desc_pkg, " | target: ", package))
  }

  if (is.null(package)) {
    return(!is.null(desc_pkg))
  }

  return(!is.null(desc_pkg) && desc_pkg == package)
}


#' Get Package Name from DESCRIPTION File
#'
#' Reads the package name from the DESCRIPTION file in the current directory.
#' This is an internal helper function used to extract package metadata without
#' requiring the package to be loaded or installed.
#'
#' @return Character string with the package name if DESCRIPTION exists and 
#'   contains a valid Package field, NULL otherwise.
#'
#' @keywords internal
.get_package_name_from_description <- function() {
  if (!file.exists("DESCRIPTION")) {
    return(NULL)
  }
  
  tryCatch({
    desc <- readLines("DESCRIPTION", warn = FALSE)
    package_line <- desc[grep("^Package:", desc)]
    if (length(package_line) > 0) {
      return(trimws(sub("^Package:", "", package_line)))
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

