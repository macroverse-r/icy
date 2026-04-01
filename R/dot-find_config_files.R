#' Find Configuration File Pairs (Internal)
#'
#' Internal function for finding paired configuration files (template and config)
#' using deterministic name-based resolution with optional fuzzy matching.
#'
#' @param package Character string with package name.
#' @param name Optional character string for named configs (e.g., "gams_switches").
#' @param fuzzy Logical. If TRUE (default), allows fuzzy matching.
#' @param confirm_fuzzy Logical. If TRUE, prompts for user confirmation for fuzzy matches.
#' @param verbose Logical. If TRUE, shows detailed messages.
#'
#' @return List with fn_tmpl and fn_config (paths or NULL).
#' @keywords internal
.find_config_files <- function(package,
                               name = NULL,
                               fuzzy = TRUE,
                               confirm_fuzzy = FALSE,
                               verbose = FALSE) {
  # Step 1: Core file searching (non-interactive)
  results <- ._find_files_core(
    package = package,
    name = name,
    fuzzy = fuzzy,
    verbose = verbose
  )

  # Step 2: Handle fuzzy match confirmation if requested
  if (confirm_fuzzy) {
    if (results$tmpl_fuzzy && !is.null(results$fn_tmpl)) {
      confirmed <- .confirm_fuzzy_match(
        original_input = "template file",
        fuzzy_match = results$fn_tmpl,
        file_type = "template"
      )
      if (!confirmed) {
        results$fn_tmpl <- NULL
        results$tmpl_fuzzy <- FALSE
      }
    }

    if (results$config_fuzzy && !is.null(results$fn_config)) {
      confirmed <- .confirm_fuzzy_match(
        original_input = "config file",
        fuzzy_match = results$fn_config,
        file_type = "config"
      )
      if (!confirmed) {
        results$fn_config <- NULL
        results$config_fuzzy <- FALSE
      }
    }
  }

  # Step 3: Return consistent structure
  return(list(
    fn_tmpl = results$fn_tmpl,
    fn_config = results$fn_config
  ))
}
