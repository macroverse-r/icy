#' Normalize Type Names
#'
#' Normalizes type names to standard R types.
#' Converts: boolean/bool -> logical, dir -> path
#'
#' @param type Character string with type name
#' @return Normalized type name
#' @keywords internal
.normalize_type <- function(type) {
  if (!is.null(type)) {
    if (type %in% c("boolean", "bool")) {
      return("logical")
    }
    if (type == "dir") {
      return("path")
    }
  }
  return(type)
}
