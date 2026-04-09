#' Find Configuration File Pairs (Internal)
#'
#' Template-first file resolution: finds the template first, then derives
#' the config filename via .swap_filename(). Config is checked by direct
#' file.exists() in the config directory. If no direct match, a fuzzy
#' search runs for the config file.
#'
#' Directory resolution happens once via ._get_config_dirs(), then
#' ._search_file() receives the resolved directories.
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

  # Accept a search result, optionally confirm fuzzy matches
  accept <- function(search, file_type) {
    if (is.null(search$path)) return(NULL)
    if (confirm_fuzzy && search$fuzzy &&
        !.confirm_fuzzy_match(paste0(file_type, " file"), search$path, file_type)) return(NULL)
    search$path
  }

  # Resolve directories once
  dirs <- ._get_config_dirs(package)

  # Step 1: Find template
  tmpl_filename <- .resolve_template_name(package, name)
  fn_tmpl <- if (!is.null(dirs$template)) {
    accept(
      ._search_file(tmpl_filename, dirs$template, fuzzy = fuzzy,
                    package = package, verbose = verbose),
      "template"
    )
  } else {
    NULL
  }

  # Step 2: Derive config from template via swap (fast path, no fuzzy needed)
  if (!is.null(fn_tmpl)) {
    config_path <- file.path(dirs$config, .swap_filename(basename(fn_tmpl)))
    if (file.exists(config_path)) {
      return(list(fn_tmpl = fn_tmpl, fn_config = normalizePath(config_path, winslash = "/")))
    }
  }

  # Step 3: Fuzzy search for config (template not found or config not at expected path)
  config_name <- if (!is.null(fn_tmpl)) .swap_filename(basename(fn_tmpl)) else .swap_filename(tmpl_filename)
  config_search <- ._search_file(
    config_name, dirs$config, fuzzy = fuzzy, package = package, verbose = verbose
  )
  fn_config <- accept(config_search, "config")
  if (!is.null(fn_config) && config_search$fuzzy) {
    .icy_warn("Config file name does not match template naming convention. Consider renaming.")
  }

  return(list(fn_tmpl = fn_tmpl, fn_config = fn_config))
}

#' Get Configuration Directories
#'
#' Returns both the config and template directory paths for a package.
#' Single .is_pkg_dir() call resolves both paths.
#'
#' @param package Character string with the package name.
#' @return Named list with `config` and `template` directory paths.
#'   `template` is NULL if the package is not found.
#' @keywords internal
._get_config_dirs <- function(package) {
  config_dir <- tools::R_user_dir(package = package, which = "config")

  if (.is_pkg_dir(package = package)) {
    tmpl_dir <- file.path(getwd(), "inst")
    if (!dir.exists(tmpl_dir)) tmpl_dir <- NULL
  } else {
    tmpl_dir <- suppressWarnings(system.file(package = package))
    if (tmpl_dir == "") {
      tmpl_dir <- NULL
    } else {
      tmpl_dir <- clean_dir_path(tmpl_dir, check_exists = FALSE)
    }
  }

  list(config = config_dir, template = tmpl_dir)
}
