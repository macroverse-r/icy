#' Define a function to get the package directory
#' 
#' The function is designed to provide the path to the git clone to developer
#' and to the installation path (similar to `fs::path_package()` or
#' `system.file()`)
#'
#' @export
get_package_dir <- function(package) {
  # First try: use current working directory if it seems to be a msgm directory
  current_dir <- getwd()
  if (basename(current_dir) == package || file.exists(file.path(current_dir, "DESCRIPTION"))) {
    return(current_dir)
  }
  
  # Second try: for installed package, use system.file
  path <- suppressWarnings(system.file(package = package))
  if (path != "") {
    # Remove any trailing "/"
    path <- sub("/$", "", path)
    # If path ends with "/inst", strip it off
    if (endsWith(path, "/inst")) {
      path <- sub("/inst$", "", path)
    }
    return(path)
  }
  
  # Fallback to current directory
  return(current_dir)
}
