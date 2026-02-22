#' Find Configuration File Pairs (Internal)
#'
#' Internal function for finding paired configuration files (template and local) with
#' optional fuzzy matching.
#'
#' @param package Character string with package name.
#' @param fn_tmpl Character string with template filename to search for.
#' @param fn_local Character string with local filename to search for.
#' @param fuzzy Logical. If TRUE (default), allows fuzzy matching.
#' @param confirm_fuzzy Logical. If TRUE, prompts for user confirmation for fuzzy matches.
#' @param case_format Character string for default filename generation.
#' @param verbose Logical. If TRUE, shows detailed messages.
#'
#' @return List with fn_tmpl and fn_local (paths or NULL).
#' @keywords internal
.find_config_files <- function(package,
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
