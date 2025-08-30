#' Find Configuration File Pairs
#'
#' Core function for finding paired configuration files (template and local) with
#' optional fuzzy matching. Always searches for both template and local files
#' as a pair, embracing icy's design philosophy.
#'
#' @param package Character string with package name. Defaults to \code{get_package_name()}.
#' @param fn_tmpl Character string with template filename to search for. Can be a partial
#'   name for fuzzy matching.
#' @param fn_local Character string with local filename to search for. Can be a partial
#'   name for fuzzy matching.
#' @param fuzzy Logical. If TRUE (default), allows fuzzy matching when exact matches
#'   are not found. Set to FALSE for exact basename matching only.
#' @param confirm_fuzzy Logical. If TRUE, prompts for user confirmation when fuzzy
#'   matches are found. Defaults to FALSE for non-interactive use.
#' @param case_format Character string for default filename generation when no filenames
#'   are provided. Options: "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param verbose Logical. If TRUE, shows detailed messages during the search process.
#'
#' @return List with components:
#'   \describe{
#'     \item{fn_tmpl}{Character string with path to template file, or NULL if not found}
#'     \item{fn_local}{Character string with path to local file, or NULL if not found}
#'   }
#'
#' @details
#' The function operates in several modes based on the provided arguments:
#' \itemize{
#'   \item If neither \code{fn_tmpl} nor \code{fn_local} are provided: uses default 
#'     patterns based on package name and case_format
#'   \item If only \code{fn_tmpl} is provided: finds template, then finds its paired local
#'   \item If only \code{fn_local} is provided: finds local, then finds its paired template
#'   \item If both are provided: finds both files independently
#' }
#'
#' When fuzzy matching is enabled and no exact match is found, the function uses
#' a similarity algorithm to find files with similar names. If \code{confirm_fuzzy = TRUE},
#' the user will be prompted to confirm the use of fuzzy matched files.
#'
#' The function always returns a consistent structure with both fn_tmpl and fn_local,
#' which will be either a valid file path or NULL. Callers should check for NULL
#' values and handle missing files according to their needs.
#'
#' @examples
#' \dontrun{
#' # Find default configuration files for a package
#' files <- find_config_files("mypackage")
#' 
#' # Find specific template and its paired local
#' files <- find_config_files("mypackage", fn_tmpl = "config_template")
#' 
#' # Find with fuzzy matching disabled (exact names only)
#' files <- find_config_files("mypackage", fn_tmpl = "template.yml", fuzzy = FALSE)
#' 
#' # Interactive mode with fuzzy match confirmation
#' files <- find_config_files("mypackage", fn_tmpl = "config", confirm_fuzzy = TRUE)
#' }
#'
#' @export
find_config_files <- function(package = get_package_name(),
                             fn_tmpl = NULL,
                             fn_local = NULL,
                             fuzzy = TRUE,
                             confirm_fuzzy = FALSE,
                             case_format = "snake_case",
                             verbose = FALSE) {
  
  # Step 1: Core file searching (non-interactive)
  results <- ._find_files_core(
    fn_tmpl = fn_tmpl,
    fn_local = fn_local,
    package = package,
    case_format = case_format,
    fuzzy = fuzzy,
    verbose = verbose
  )
  
  # Step 2: Handle fuzzy match confirmation if requested
  if (confirm_fuzzy && (results$tmpl_fuzzy || results$local_fuzzy)) {
    results <- ._handle_fuzzy_confirmation(
      results = results,
      package = package,
      verbose = verbose
    )
  }
  
  # Step 3: Return consistent structure
  return(list(
    fn_tmpl = results$fn_tmpl,
    fn_local = results$fn_local
  ))
}