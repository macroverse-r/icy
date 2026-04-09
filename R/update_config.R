#' Update Variables in Configuration
#'
#' Writes or updates variables in the configuration YAML file.
#' Only variables defined in the template are kept; any other variables
#' are removed to ensure consistency with the template.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()`
#'   to detect the calling package.
#' @param var_list Named list of variables to write. Names should be the
#'   variable names and values should be the values to set.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param name Optional character string for named configs (e.g., "gams_switches").
#'   If NULL (default), writes to the main config file (\{package\}_config.yml).
#' @param create_if_missing Logical; if TRUE (default), creates the config file
#'   from template if it doesn't exist. If FALSE, returns an error if file is missing.
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
#' @examples
#' \dontrun{
#' # Write variables to config
#' update_config(
#'   var_list = list(
#'     API_KEY = "my-secret-key",
#'     DB_HOST = "localhost",
#'     DEBUG_MODE = "TRUE"
#'   ),
#'   package = "mypackage"
#' )
#'
#' # Write to a specific section
#' update_config(
#'   var_list = list(API_URL = "https://prod.api.com"),
#'   package = "mypackage",
#'   section = "production"
#' )
#' }
#'
#' @export
update_config <- function(package = get_package_name(),
                         var_list,
                         section = "default",
                         name = NULL,
                         create_if_missing = TRUE,
                         verbose = FALSE,
                         sync = "conservative") {

  # Input validation
  if (is.character(var_list)) {
    .icy_stop(paste0(
      "var_list must be a named list, not a character string. ",
      "Example: update_config(package = \"", var_list[1], "\", var_list = list(VAR = \"value\"))"
    ))
  }

  if (!is.list(var_list) || length(var_list) == 0) {
    .icy_stop("var_list must be a non-empty named list")
  }

  if (is.null(names(var_list)) || any(names(var_list) == "")) {
    .icy_stop("All elements in var_list must be named")
  }

  # Capture current session variables before any changes
  original_session_vars <- .get_current_session_vars(package, section)

  # Find or create config file
  resolved_files <- .find_config_files(
    package = package,
    name = name,
    verbose = FALSE
  )
  config_path <- resolved_files$fn_config
  template_path <- resolved_files$fn_tmpl

  # Create if missing
  if (is.null(config_path)) {
    if (create_if_missing) {
      config_path <- create_config(
        package = package,
        name = name
      )
      if (verbose) {
        .icy_alert(paste0("Created new config file: ", config_path))
      }
    } else {
      .icy_stop(c(
        paste0("No configuration file found for package ", package),
        "i" = "Set create_if_missing = TRUE to create one automatically"
      ))
    }
  }

  # Track what we're changing for verbose output
  if (verbose && file.exists(config_path)) {
    existing_config <- yaml::read_yaml(config_path)
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
    file_path = config_path,
    package = package,
    section = section,
    template_file = template_path,
    create_if_missing = FALSE,  # Already handled above
    custom_header = .generate_header(package, type = "config"),
    append_sections = TRUE,     # Always merge for configs
    strict_template = TRUE,     # Remove non-template vars
    verbose = FALSE            # We handle our own messages
  )

  # Report what was done (if verbose)
  if (verbose && exists("updated_vars") && exists("new_vars")) {
    if (length(updated_vars) > 0) {
      .icy_success(paste0("Updated ", length(updated_vars), " variable",
                         if(length(updated_vars) > 1) "s" else "", " in config"))
      bullets <- updated_vars
      names(bullets) <- rep("*", length(updated_vars))
      .icy_bullets(bullets)
    }

    if (length(new_vars) > 0) {
      .icy_success(paste0("Added ", length(new_vars), " new variable",
                         if(length(new_vars) > 1) "s" else "", " to config"))
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
