#' Write Environment Variables to Local Configuration
#'
#' Writes or updates environment variables in the local configuration YAML file.
#' This is a streamlined implementation that delegates the actual YAML writing
#' to the generalized write_config_yaml function. Only variables defined in the
#' template are kept; any other variables in the local config are removed to
#' ensure consistency with the template.
#'
#' @param var_list Named list of environment variables to write. Names should be the
#'   environment variable names and values should be the values to set.
#' @param package Character string with the package name. Defaults to `get_package_name()`
#'   to detect the calling package.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param fn_local Character string with custom filename for the local config.
#'   If NULL, uses the default naming pattern.
#' @param create_if_missing Logical; if TRUE (default), creates the local config file
#'   from template if it doesn't exist. If FALSE, returns an error if file is missing.
#' @param case_format Character string indicating the case format to use for filenames.
#'   Options are: "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param fn_tmpl Character string with the name or path to a custom YAML template file
#'   for validation. If NULL (default), uses the standard template file for the package.
#' @param verbose Logical. If TRUE, displays informative messages about the operation. 
#'   Defaults to FALSE.
#' @param sync Character or logical. Controls session environment synchronization:
#'   - "conservative" (default): only sync variables already in session
#'   - "all" or TRUE: sync all written variables to session  
#'   - "none" or FALSE: skip synchronization
#'   - character vector: explicit list of variables to sync
#'
#' @return Invisibly returns NULL on success.
#'
#' @details
#' This function enforces template consistency by:
#' \itemize{
#'   \item Validating all variables against the template
#'   \item Removing any variables not defined in the template
#'   \item Ordering variables to match the template order
#'   \item Preserving NULL values as defined in the template
#' }
#'
#' Variables not in the template will be silently removed from the local
#' configuration to prevent undefined variables from accumulating. This ensures
#' local configs remain clean subsets of the template definition.
#'
#' @examples
#' \dontrun{
#' # Write variables to local config
#' write_local(
#'   var_list = list(
#'     API_KEY = "my-secret-key",
#'     DB_HOST = "localhost",
#'     DEBUG_MODE = "TRUE"
#'   ),
#'   package = "mypackage"
#' )
#'
#' # Write to a specific section
#' write_local(
#'   var_list = list(API_URL = "https://prod.api.com"),
#'   package = "mypackage",
#'   section = "production"
#' )
#' }
#'
#' @export
write_local <- function(var_list,
                        package = get_package_name(),
                        section = "default",
                        fn_local = NULL,
                        create_if_missing = TRUE,
                        case_format = "snake_case",
                        fn_tmpl = NULL,
                        verbose = FALSE,
                        sync = "conservative") {
  
  # Input validation
  if (!is.list(var_list) || length(var_list) == 0) {
    .icy_stop("var_list must be a non-empty named list of environment variables")
  }
  
  if (is.null(names(var_list)) || any(names(var_list) == "")) {
    .icy_stop("All elements in var_list must be named")
  }
  
  # Capture current session variables before any changes
  original_session_vars <- .get_current_session_vars(package, section)
  
  # Find or create local config file
  local_path <- find_local(
    package = package,
    fn_local = fn_local,
    case_format = case_format,
    verbose = FALSE
  )
  
  # Create if missing
  if (is.null(local_path)) {
    if (create_if_missing) {
      local_path <- create_local(
        package = package,
        fn_local = fn_local,
        case_format = case_format
      )
      if (verbose) {
        .icy_alert(paste0("Created new local config file: ", local_path))
      }
    } else {
      .icy_stop(c(
        paste0("No local configuration file found for package ", package),
        "i" = "Set create_if_missing = TRUE to create one automatically"
      ))
    }
  }
  
  # Track what we're changing for verbose output
  if (verbose && file.exists(local_path)) {
    existing_config <- yaml::read_yaml(local_path)
    if (section %in% names(existing_config)) {
      existing_vars <- names(existing_config[[section]])
      new_vars <- setdiff(names(var_list), existing_vars)
      updated_vars <- intersect(names(var_list), existing_vars)
    } else {
      new_vars <- names(var_list)
      updated_vars <- character(0)
    }
  }
  
  # Call the generalized YAML writer
  .write_config_yaml(
    var_list = var_list,
    file_path = local_path,
    package = package,
    section = section,
    template_file = fn_tmpl,
    create_if_missing = FALSE,  # Already handled above
    custom_header = .generate_header(package, type = "local"),  # Preserve local headers
    append_sections = TRUE,     # Always merge for local configs
    strict_template = TRUE,     # Remove non-template vars (matches write_local)
    verbose = FALSE            # We handle our own messages
  )
  
  # Report what was done (if verbose)
  if (verbose && exists("updated_vars") && exists("new_vars")) {
    if (length(updated_vars) > 0) {
      .icy_success(paste0("Updated ", length(updated_vars), " variable", 
                         if(length(updated_vars) > 1) "s" else "", " in local config"))
      bullets <- updated_vars
      names(bullets) <- rep("*", length(updated_vars))
      .icy_bullets(bullets)
    }
    
    if (length(new_vars) > 0) {
      .icy_success(paste0("Added ", length(new_vars), " new variable", 
                         if(length(new_vars) > 1) "s" else "", " to local config"))
      bullets <- new_vars
      names(bullets) <- rep("*", length(new_vars))
      .icy_bullets(bullets)
    }
    
    if (length(updated_vars) == 0 && length(new_vars) == 0) {
      .icy_alert("No changes made - all values were already up to date")
    }
  }
  
  # Apply sync logic to session environment variables
  if (length(var_list) > 0) {
    synced_vars <- .apply_sync(var_list, sync, original_session_vars, verbose = verbose)
  }
  
  return(invisible(NULL))
}