#' Unified File Finding Function
#'
#' Core function for finding configuration files with optional fuzzy match confirmation
#' and automatic pairing. Provides a consistent interface for all file finding operations
#' in the icy package.
#'
#' @param package Character string with package name. Defaults to \code{get_package_name()}.
#' @param fn_tmpl Character string with template filename to search for. Can be a partial
#'   name for fuzzy matching.
#' @param fn_local Character string with local filename to search for. Can be a partial
#'   name for fuzzy matching.
#' @param pairing Logical. If TRUE (default), automatically finds the corresponding paired
#'   file when only one filename is provided. Must be TRUE if both filenames are provided.
#' @param confirm_fuzzy Logical. If TRUE, asks user to confirm fuzzy matches interactively.
#'   Defaults to FALSE for non-interactive use.
#' @param case_format Character string for default filename generation when no filenames
#'   are provided. Options: "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param exact Logical. If TRUE, only allows exact basename matches (no fuzzy matching).
#'   Defaults to FALSE for normal fuzzy searching behavior.
#' @param verbose Logical. If TRUE, shows detailed messages during the search process.
#'
#' @return List with components:
#'   \describe{
#'     \item{fn_tmpl}{Character string with path to template file, or NULL if not found}
#'     \item{fn_local}{Character string with path to local file, or NULL if not found}
#'     \item{source}{Character string indicating search result: "exact_match", "fuzzy_match",
#'       "not_found", or "pair_found"}
#'   }
#'
#' @details
#' The function operates in several modes based on the provided arguments:
#' \itemize{
#'   \item If both \code{fn_tmpl} and \code{fn_local} are provided, searches for both files
#'   \item If only \code{fn_tmpl} is provided: finds template, optionally pairs with local
#'   \item If only \code{fn_local} is provided: finds local, optionally pairs with template
#'   \item If neither is provided: uses default patterns to find both files
#' }
#'
#' When \code{pairing = TRUE} and only one file is specified, the function automatically
#' generates the corresponding filename using the pairing rules (e.g., "template" ↔ "local").
#'
#' The \code{source} field in the return value indicates how the primary file was found:
#' \itemize{
#'   \item "exact_match": File found with exact name match
#'   \item "fuzzy_match": File found using fuzzy matching
#'   \item "not_found": No matching file found
#'   \item "pair_found": Both files found (when searching for defaults)
#' }
#'
#' @examples
#' \dontrun{
#' # Find template and automatically find its paired local
#' find_file("mypackage", fn_tmpl = "config_template")
#' 
#' # Find local file only (no pairing)
#' find_file("mypackage", fn_local = "config_local", pairing = FALSE)
#' 
#' # Find both with explicit names
#' find_file("mypackage", fn_tmpl = "my_template", fn_local = "my_local")
#' 
#' # Find defaults for package
#' find_file("mypackage")
#' 
#' # Interactive fuzzy matching
#' find_file("mypackage", fn_local = "conf", confirm_fuzzy = TRUE)
#' }
#'
#' @seealso \code{\link{find_local}}, \code{\link{find_template}}
#' @export
find_file <- function(package = get_package_name(),
                     fn_tmpl = NULL,
                     fn_local = NULL,
                     pairing = TRUE,
                     confirm_fuzzy = FALSE,
                     case_format = "snake_case",
                     exact = FALSE,
                     verbose = FALSE) {
  
  # Validation: if both provided, pairing must be TRUE
  if (!is.null(fn_tmpl) && !is.null(fn_local) && !pairing) {
    .icy_stop("When both fn_tmpl and fn_local are provided, pairing must be TRUE")
  }
  
  # Case 1: Both files provided - use existing pairing infrastructure
  if (!is.null(fn_tmpl) && !is.null(fn_local)) {
    if (confirm_fuzzy) {
      result <- .validate_file_pairing(
        fn_tmpl = fn_tmpl,
        fn_local = fn_local,
        package = package,
        verbose = verbose
      )
    } else {
      result <- .check_file_pairing(
        fn_tmpl = fn_tmpl,
        fn_local = fn_local,
        package = package,
        verbose = verbose
      )
      # Map various statuses to our simplified source
      if (result$status %in% c("both_provided", "pair_found", "use_defaults")) {
        result$source <- "pair_found"
      } else if (result$status %in% c("template_fuzzy_match", "local_fuzzy_match")) {
        result$source <- "fuzzy_match"
      } else if (result$status %in% c("template_not_found", "local_not_found")) {
        result$source <- "not_found"
      } else {
        result$source <- "exact_match"
      }
    }
    
    # Return NULL for files that don't exist, actual paths for files that do
    tmpl_return <- if (result$status == "template_not_found") NULL else result$fn_tmpl
    local_return <- if (result$status == "local_not_found") NULL else result$fn_local
    
    return(list(
      fn_tmpl = tmpl_return,
      fn_local = local_return,
      source = result$source
    ))
  }
  
  # Case 2: Only template provided
  if (!is.null(fn_tmpl) && is.null(fn_local)) {
    # Find template first
    tmpl_result <- .check_single_file(
      filename = fn_tmpl,
      package = package,
      is_template = TRUE,
      exact = exact,
      verbose = verbose
    )
    
    # Handle fuzzy confirmation if needed
    tmpl_path <- NULL
    tmpl_status <- tmpl_result$status
    
    if (tmpl_result$status == "fuzzy_match" && confirm_fuzzy) {
      confirmed <- .confirm_fuzzy_match(
        original_input = fn_tmpl,
        fuzzy_match = tmpl_result$path,
        file_type = "template",
        param_name = "fn_tmpl"
      )
      if (is.null(confirmed)) {
        return(list(fn_tmpl = NULL, fn_local = NULL, source = "not_found"))
      }
      tmpl_path <- confirmed
    } else if (tmpl_result$status != "not_found") {
      tmpl_path <- tmpl_result$path
    } else {
      return(list(fn_tmpl = NULL, fn_local = NULL, source = "not_found"))
    }
    
    # If pairing, find corresponding local
    local_path <- NULL
    if (pairing && !is.null(tmpl_path)) {
      # Only attempt pairing if we found a valid YAML file
      if (grepl("\\.ya?ml$", tmpl_path, ignore.case = TRUE)) {
        local_name <- .generate_corresponding_file(basename(tmpl_path), "local")
        local_result <- .check_single_file(
          filename = local_name,
          package = package,
          is_template = FALSE,
          exact = exact,
          verbose = verbose
        )
        
        if (local_result$status != "not_found") {
          local_path <- local_result$path
        }
      }
    }
    
    return(list(
      fn_tmpl = tmpl_path,
      fn_local = local_path,
      source = tmpl_status  # Use template's status as primary
    ))
  }
  
  # Case 3: Only local provided (mirror of Case 2)
  if (is.null(fn_tmpl) && !is.null(fn_local)) {
    # Find local first
    local_result <- .check_single_file(
      filename = fn_local,
      package = package,
      is_template = FALSE,
      exact = exact,
      verbose = verbose
    )
    
    # Handle fuzzy confirmation if needed
    local_path <- NULL
    local_status <- local_result$status
    
    if (local_result$status == "fuzzy_match" && confirm_fuzzy) {
      confirmed <- .confirm_fuzzy_match(
        original_input = fn_local,
        fuzzy_match = local_result$path,
        file_type = "local",
        param_name = "fn_local"
      )
      if (is.null(confirmed)) {
        return(list(fn_tmpl = NULL, fn_local = NULL, source = "not_found"))
      }
      local_path <- confirmed
    } else if (local_result$status != "not_found") {
      local_path <- local_result$path
    } else {
      return(list(fn_tmpl = NULL, fn_local = NULL, source = "not_found"))
    }
    
    # If pairing, find corresponding template
    tmpl_path <- NULL
    if (pairing && !is.null(local_path)) {
      # Only attempt pairing if we found a valid YAML file
      if (grepl("\\.ya?ml$", local_path, ignore.case = TRUE)) {
        tmpl_name <- .generate_corresponding_file(basename(local_path), "template")
        tmpl_result <- .check_single_file(
          filename = tmpl_name,
          package = package,
          is_template = TRUE,
          exact = exact,
          verbose = verbose
        )
        
        if (tmpl_result$status != "not_found") {
          tmpl_path <- tmpl_result$path
        }
      }
    }
    
    return(list(
      fn_tmpl = tmpl_path,
      fn_local = local_path,
      source = local_status  # Use local's status as primary
    ))
  }
  
  # Case 4: Neither provided - use defaults
  if (is.null(fn_tmpl) && is.null(fn_local)) {
    if (!pairing) {
      .icy_stop("When no filenames provided, pairing must be TRUE to find default files")
    }
    
    # Generate default filenames (not regex patterns)
    default_tmpl <- .pattern(
      package = package,
      case_format = case_format,
      file = "template",
      yml = TRUE  # Get actual filename, not regex
    )
    default_local <- .pattern(
      package = package,
      case_format = case_format,
      file = "local",
      yml = TRUE  # Get actual filename, not regex
    )
    
    # Check for template directly
    tmpl_result <- .check_single_file(
      filename = default_tmpl,
      package = package,
      is_template = TRUE,
      exact = exact,
      verbose = verbose
    )
    
    # Handle fuzzy confirmation for template if needed
    tmpl_path <- NULL
    if (tmpl_result$status == "fuzzy_match" && confirm_fuzzy) {
      confirmed <- .confirm_fuzzy_match(
        original_input = default_tmpl,
        fuzzy_match = tmpl_result$path,
        file_type = "template",
        param_name = "fn_tmpl"
      )
      tmpl_path <- confirmed  # Will be NULL if user declines
    } else if (tmpl_result$status != "not_found") {
      tmpl_path <- tmpl_result$path
    }
    
    # Check for local directly
    local_result <- .check_single_file(
      filename = default_local,
      package = package,
      is_template = FALSE,
      exact = exact,
      verbose = verbose
    )
    
    # Handle fuzzy confirmation for local if needed
    local_path <- NULL
    if (local_result$status == "fuzzy_match" && confirm_fuzzy) {
      confirmed <- .confirm_fuzzy_match(
        original_input = default_local,
        fuzzy_match = local_result$path,
        file_type = "local",
        param_name = "fn_local"
      )
      local_path <- confirmed  # Will be NULL if user declines
    } else if (local_result$status != "not_found") {
      local_path <- local_result$path
    }
    
    # Determine source based on results
    if (!is.null(tmpl_path) && !is.null(local_path)) {
      if (tmpl_result$status == "fuzzy_match" || local_result$status == "fuzzy_match") {
        source <- "fuzzy_match"
      } else {
        source <- "pair_found"
      }
    } else if (!is.null(tmpl_path) || !is.null(local_path)) {
      if (tmpl_result$status == "fuzzy_match" || local_result$status == "fuzzy_match") {
        source <- "fuzzy_match"
      } else {
        source <- "exact_match"
      }
    } else {
      source <- "not_found"
    }
    
    return(list(fn_tmpl = tmpl_path, fn_local = local_path, source = source))
  }
}