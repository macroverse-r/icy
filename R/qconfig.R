#' Interactive Configuration Variable Selection and Writing
#'
#' Prompts the user to configure a specific environment variable with options
#' from the template YAML file and/or user-provided options, then writes the
#' selected value to the specified configuration location. This function
#' integrates with icy's template system to provide descriptions and predefined
#' options for configuration variables.
#'
#' @param var_name Character string with the environment variable name (e.g., "DUMMY_API_KEY").
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param description Character string with custom description. If NULL, uses description
#'   from template YAML descriptions section. If no template description exists, no description is shown.
#' @param options Vector of option values to present to the user. If NULL, uses options
#'   from template YAML options section. If both are provided, argument options come first,
#'   followed by template options (unless arg_only = TRUE).
#' @param allow_skip Logical. If TRUE, allows user to skip configuration by pressing Enter
#'   without selecting an option. Returns NULL when skipped. Defaults to TRUE.
#' @param note Character string with additional informational note to display before options.
#' @param arg_only Logical. If TRUE, only uses options from the argument and ignores template
#'   options. If FALSE (default), merges argument and template options.
#' @param write Character string specifying where to write the configuration.
#'   Options: "local" (default, writes to local YAML config), "renviron" (writes to ~/.Renviron),
#'   "session" (sets in current R session only using Sys.setenv).
#' @param type Character string specifying the expected type for the variable.
#'   If NULL (default), uses type from template file. If not found in template, keeps value as-is.
#'   Possible values: "character", "integer", "numeric", "logical", "path", "dir".
#'   For "path"/"dir" types, directory paths are automatically cleaned and validated using clean_dir_path().
#' @param allow_custom Logical. Whether to allow custom input when predefined options exist.
#'   If NULL (default), uses smart defaults: FALSE for "logical" types, TRUE for all other types.
#'   When TRUE, users can enter 'c' to provide custom input alongside predefined options.
#' @param allow_create_dir Logical. Whether to allow interactive directory creation for path types.
#'   Defaults to TRUE. When TRUE and type is "path", users are prompted to create non-existent directories.
#'   When FALSE, non-existent paths result in retry prompts. Only affects "path" types.
#' @param resolve_paths Character string specifying how path keywords should be resolved.
#'   Options: "ask" (default, prompt user for static vs dynamic), "static" (resolve immediately),
#'   "dynamic" (store keywords for runtime resolution). Users can also use suffix notation:
#'   "8s" for static, "8d" for dynamic, "8" to ask (when resolve_paths="ask").
#' @param fn_tmpl Character string with the name or path to a custom YAML template file.
#'   If NULL (default), uses the standard template file for the package. Must be specified
#'   together with fn_local when using custom template files.
#' @param fn_local Character string with the name or path to a custom local YAML config file.
#'   If NULL (default), uses the standard local config file for the package. Must be specified
#'   together with fn_tmpl when using custom configuration files.
#' @param verbose Logical. If TRUE, displays confirmation messages. Defaults to FALSE.
#'
#' @return The selected option value, or NULL if allow_skip = TRUE and user skips.
#'   If no options are available (neither argument nor template), prompts for manual text input.
#'   The value is written to the specified configuration location before being returned.
#'
#' @details
#' This function follows the pattern established by MSGM's ask_for_directory function.
#' It integrates with icy's template system to provide:
#' \itemize{
#'   \item Automatic descriptions from template descriptions section
#'   \item Predefined options from template options section  
#'   \item Manual text input fallback when no options are available
#'   \item Skip functionality for optional configuration
#' }
#'
#' Priority for descriptions: argument description > template description > no description
#' Priority for options: argument options + template options (argument first), or arg_only for argument only
#'
#' @examples
#' \dontrun{
#' # Basic usage with template integration (writes to local config)
#' api_key <- qconfig("DUMMY_API_KEY", package = "dummy")
#' # Uses template description and options, writes to local YAML config
#'
#' # Write to ~/.Renviron instead
#' port <- qconfig("DUMMY_DB_PORT", write = "renviron")
#' # Writes selected value to ~/.Renviron file
#'
#' # Set only in current session
#' log_level <- qconfig("DUMMY_LOG_LEVEL", write = "session")
#' # Sets value using Sys.setenv() for current session only
#'
#' # Add custom options to template options
#' timeout <- qconfig("DUMMY_TIMEOUT", options = c("30", "60", "120"))
#' # Shows custom options first, then template options, writes to local config
#'
#' # Skip functionality - no writing occurs
#' optional_var <- qconfig("DUMMY_OPTIONAL", allow_skip = TRUE)
#' # Can return NULL if user skips, no configuration is written
#'
#' # Directory path configuration with automatic cleaning
#' data_dir <- qconfig("DUMMY_DATA_DIR", type = "path")
#' # Shows "Type: Directory path", validates and cleans user input
#' # User enters: "/home/user/data/" -> cleaned to: "/home/user/data"
#'
#' # Directory path with predefined options 
#' cache_dir <- qconfig("DUMMY_CACHE_DIR", type = "dir", 
#'                      options = c("/tmp/cache", "~/.cache", "/var/cache"))
#' # Selected option is automatically cleaned and validated
#' # User can also enter 'c' to provide custom directory path (default for path types)
#'
#' # Boolean type with strict options only
#' verbose <- qconfig("DUMMY_VERBOSE", options = c("TRUE", "FALSE"))
#' # No custom input allowed by default for logical types
#'
#' # Force custom input for any type
#' api_key <- qconfig("DUMMY_API_KEY", allow_custom = TRUE)
#' # Even with predefined options, user can enter 'c' for custom value
#'
#' # Directory creation control
#' data_dir <- qconfig("DATA_DIR", type = "path")  # allow_create_dir = TRUE (default)
#' # User prompted to create non-existent directories
#'
#' strict_dir <- qconfig("LOG_DIR", type = "path", allow_create_dir = FALSE)
#' # User must provide existing directories only
#' }
#' @export
qconfig <- function(var_name, package = get_package_name(), user = "default",
                    description = NULL, options = NULL, allow_skip = TRUE, 
                    note = NULL, arg_only = FALSE, write = "local", type = NULL, 
                    allow_custom = NULL, allow_create_dir = TRUE, resolve_paths = "ask", 
                    fn_tmpl = NULL, fn_local = NULL, verbose = FALSE) {
  
  # Validate and normalize parameters
  params <- .validate_and_normalize_qconfig_params(
    var_name, package, user, description, options, allow_skip, 
    note, arg_only, write, type, allow_custom, allow_create_dir, resolve_paths, fn_tmpl, fn_local, verbose
  )
  
  # Read template data using modular functions 
  template_description <- .get_description(params$var_name, params$package, params$fn_tmpl)
  template_type <- .get_type(params$var_name, params$package, params$fn_tmpl)  # Already normalized boolean→logical
  template_options <- .get_option(params$var_name, params$package, params$fn_tmpl)
  template_note <- .get_note(params$var_name, params$package, params$fn_tmpl)
  
  # Determine final values (argument > template > none)
  final_description <- if (!is.null(params$description)) params$description else template_description
  final_type <- if (!is.null(params$type)) params$type else template_type
  final_note <- if (!is.null(params$note)) params$note else template_note
  
  # Apply automatic boolean behavior AFTER reading template type
  if (!is.null(final_type) && final_type == "logical" && is.null(params$options)) {
    # Automatically set TRUE/FALSE options for boolean types
    final_options <- c("TRUE", "FALSE")
    arg_only <- TRUE  # Force arg_only for boolean types to prevent template conflicts
  } else {
    # Determine final options (merge or arg_only)
    final_options <- NULL
    if (!is.null(params$options)) {
      final_options <- as.character(params$options)
      if (!params$arg_only && !is.null(template_options)) {
        # Merge: argument options first, then template options (remove duplicates)
        final_options <- unique(c(final_options, template_options))
      }
    } else if (!is.null(template_options)) {
      final_options <- template_options
    }
    arg_only <- params$arg_only
  }
  
  # Determine final allow_custom setting
  final_allow_custom <- .determine_allow_custom(final_type, params$allow_custom)
  
  # Perform interactive configuration (pass final_type for display)
  raw_result <- .do_interactive_config(params$var_name, final_description, final_options, 
                                       params$allow_skip, final_note, params$write, 
                                       params$package, params$user, params$verbose, final_type, 
                                       final_allow_custom, params$allow_create_dir, params$resolve_paths, params$fn_tmpl, params$fn_local)
  
  # Convert to proper type and return
  return(.convert_return_value(raw_result, final_type))
}
