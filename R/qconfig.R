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
#' @param section Character string for the section in the YAML file (default: "default").
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
qconfig <- function(var_name, package = get_package_name(), section = "default",
                    description = NULL, options = NULL, allow_skip = TRUE, 
                    note = NULL, arg_only = FALSE, write = "local", type = NULL, 
                    allow_custom = NULL, allow_create_dir = TRUE, resolve_paths = "ask", 
                    fn_tmpl = NULL, fn_local = NULL, verbose = FALSE) {
  
  # Validate and normalize parameters
  params <- ._qconfig_validate_and_normalize_params(
    var_name, package, section, description, options, allow_skip, 
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
  final_allow_custom <- ._qconfig_determine_allow_custom(final_type, params$allow_custom)
  
  # Perform interactive configuration (pass final_type for display)
  raw_result <- ._qconfig_do_interactive_config(params$var_name, final_description, final_options, 
                                       params$allow_skip, final_note, params$write, 
                                       params$package, params$section, params$verbose, final_type, 
                                       final_allow_custom, params$allow_create_dir, params$resolve_paths, params$fn_tmpl, params$fn_local)
  
  # Convert to proper type and return
  return(._qconfig_convert_by_type(raw_result, final_type))
}


# Internal functions used only by qconfig() ----

#' Validate and Normalize qconfig Parameters
#' @keywords internal
._qconfig_validate_and_normalize_params <- function(var_name, package, section, description, options, allow_skip, note, arg_only, write, type, allow_custom, allow_create_dir, resolve_paths, fn_tmpl, fn_local, verbose) {
  # Input validation
  if (!is.character(var_name) || length(var_name) != 1 || nchar(var_name) == 0) {
    .icy_stop("var_name must be a non-empty character string")
  }
  
  if (!is.character(package) || length(package) != 1 || nchar(package) == 0) {
    .icy_stop("package must be a non-empty character string")
  }
  
  if (!is.character(section) || length(section) != 1 || nchar(section) == 0) {
    .icy_stop("section must be a non-empty character string")
  }
  
  if (!is.null(description) && (!is.character(description) || length(description) != 1)) {
    .icy_stop("description must be NULL or a character string")
  }
  
  if (!is.null(options) && !is.vector(options)) {
    .icy_stop("options must be NULL or a vector")
  }
  
  if (!is.logical(allow_skip) || length(allow_skip) != 1 || is.na(allow_skip)) {
    .icy_stop("allow_skip must be TRUE or FALSE")
  }
  
  if (!is.null(note) && (!is.character(note) || length(note) != 1)) {
    .icy_stop("note must be NULL or a character string")
  }
  
  if (!is.logical(arg_only) || length(arg_only) != 1 || is.na(arg_only)) {
    .icy_stop("arg_only must be TRUE or FALSE")
  }
  
  if (!is.character(write) || length(write) != 1 || !write %in% c("local", "renviron", "session")) {
    .icy_stop("write must be one of: 'local', 'renviron', 'session'")
  }
  
  if (!is.null(type) && (!is.character(type) || length(type) != 1 || !type %in% c("character", "integer", "numeric", "logical", "boolean", "dir", "path"))) {
    .icy_stop("type must be NULL or one of: 'character', 'integer', 'numeric', 'logical', 'boolean', 'dir', 'path'")
  }
  
  if (!is.null(allow_custom) && (!is.logical(allow_custom) || length(allow_custom) != 1 || is.na(allow_custom))) {
    .icy_stop("allow_custom must be NULL, TRUE or FALSE")
  }
  
  if (!is.logical(allow_create_dir) || length(allow_create_dir) != 1 || is.na(allow_create_dir)) {
    .icy_stop("allow_create_dir must be TRUE or FALSE")
  }
  
  if (!is.character(resolve_paths) || length(resolve_paths) != 1 || !resolve_paths %in% c("ask", "static", "dynamic")) {
    .icy_stop("resolve_paths must be one of: 'ask', 'static', 'dynamic'")
  }
  
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    .icy_stop("verbose must be TRUE or FALSE")
  }
  
  # Unified file validation and pairing
  paired_files <- .validate_file_pairing(
    fn_tmpl = fn_tmpl,
    fn_local = fn_local,
    package = package,
    section = section,
    verbose = verbose
  )
  
  # Update parameters with validated files
  fn_tmpl <- paired_files$fn_tmpl
  fn_local <- paired_files$fn_local
  
  # Validate fn_tmpl and fn_local types if specified
  if (!is.null(fn_tmpl) && (!is.character(fn_tmpl) || length(fn_tmpl) != 1 || nchar(fn_tmpl) == 0)) {
    .icy_stop("fn_tmpl must be a non-empty character string")
  }
  if (!is.null(fn_local) && (!is.character(fn_local) || length(fn_local) != 1 || nchar(fn_local) == 0)) {
    .icy_stop("fn_local must be a non-empty character string")
  }
  
  # Normalize type parameter
  if (!is.null(type) && type %in% c("boolean", "bool")) {
    type <- "logical"
  }
  if (!is.null(type) && type == "dir") {
    type <- "path"
  }
  
  return(list(
    var_name = var_name,
    package = package,
    section = section,
    description = description,
    options = options,
    allow_skip = allow_skip,
    note = note,
    arg_only = arg_only,
    write = write,
    type = type,
    allow_custom = allow_custom,
    allow_create_dir = allow_create_dir,
    resolve_paths = resolve_paths,
    fn_tmpl = fn_tmpl,
    fn_local = fn_local,
    verbose = verbose
  ))
}

#' Determine Allow Custom Setting
#' @keywords internal
._qconfig_determine_allow_custom <- function(type, allow_custom) {
  if (!is.null(allow_custom)) {
    return(allow_custom)
  }
  
  if (is.null(type)) {
    return(TRUE)
  }
  
  switch(type,
    "logical" = FALSE,
    "path" = TRUE,
    "character" = TRUE,
    "integer" = TRUE,
    "numeric" = TRUE,
    TRUE
  )
}

#' Convert Value by Type
#' @keywords internal
._qconfig_convert_by_type <- function(value, type) {
  if (is.null(type)) {
    return(value)
  }
  
  switch(type,
    "character" = as.character(value),
    "integer" = {
      converted <- suppressWarnings(as.integer(value))
      if (is.na(converted)) value else converted
    },
    "numeric" = {
      converted <- suppressWarnings(as.numeric(value))
      if (is.na(converted)) value else converted
    },
    "logical" = {
      if (is.na(value) || value == "") {
        return(NA)
      }
      
      lower_value <- tolower(trimws(value))
      
      if (lower_value %in% c("yes", "true", "t", "y", "on", "1")) {
        TRUE
      } else if (lower_value %in% c("no", "false", "f", "n", "off", "0")) {
        FALSE
      } else {
        value
      }
    },
    value
  )
}

#' Parse Selection Input with Resolution Mode Suffixes
#' @keywords internal
._qconfig_parse_selection_input <- function(input) {
  input <- trimws(input)
  
  if (grepl("^[0-9]+[sd]$", input)) {
    number <- as.integer(gsub("[sd]$", "", input))
    mode <- if (endsWith(input, "s")) "static" else "dynamic"
    return(list(selection = number, mode = mode))
  }
  
  number <- suppressWarnings(as.integer(input))
  return(list(selection = number, mode = "ask"))
}

#' Handle Skip Input
#' @keywords internal
._qconfig_handle_skip_input <- function(user_input, allow_skip) {
  if (allow_skip && nchar(user_input) == 0) {
    .icy_inform("Skipped configuration")
    return(NULL)
  }
  return(FALSE)
}

#' Validate Input Type
#' @keywords internal
._qconfig_validate_input_type <- function(value, type) {
  if (is.null(type)) {
    return(NULL)
  }
  
  if (type == "character") {
    return(NULL)
  }
  
  switch(type,
    "integer" = {
      numeric_val <- suppressWarnings(as.numeric(value))
      if (is.na(numeric_val)) {
        return(paste0("'", value, "' is not a valid integer"))
      }
      
      integer_val <- suppressWarnings(as.integer(numeric_val))
      if (is.na(integer_val)) {
        return(paste0("'", value, "' is too large for an integer (max: ", .Machine$integer.max, ")"))
      }
      
      if (numeric_val != integer_val) {
        return(paste0("'", value, "' is not a valid integer (decimals not allowed)"))
      }
      NULL
    },
    "numeric" = {
      converted <- suppressWarnings(as.numeric(value))
      if (is.na(converted)) {
        return(paste0("'", value, "' is not a valid number"))
      }
      NULL
    },
    "logical" = {
      if (!value %in% c("yes", "true", "TRUE", "True", "on", "1", 
                        "no", "false", "FALSE", "False", "off", "0")) {
        return(paste0("'", value, "' is not a valid boolean value. Use: TRUE/FALSE, yes/no, 1/0, true/false, on/off"))
      }
      NULL
    },
    NULL
  )
}

#' Resolve Special Path Keywords
#' @keywords internal
._qconfig_resolve_special_path <- function(path_string, package = NULL) {
  .resolve_keyword <- function(keyword) {
    switch(keyword,
      "home" = path.expand("~"),
      "cache" = tools::R_user_dir(package, "cache"),
      "config" = tools::R_user_dir(package, "config"), 
      "data" = tools::R_user_dir(package, "data"),
      "tempdir" = tempdir(),
      "getwd" = getwd(),
      "." = getwd(),
      ".." = dirname(getwd()),
      keyword
    )
  }
  
  if (grepl("[|/\\\\]", path_string)) {
    if (grepl("\\|", path_string)) {
      parts <- strsplit(path_string, "\\|")[[1]]
    } else if (grepl("/", path_string)) {
      parts <- strsplit(path_string, "/")[[1]]
    } else if (grepl("\\\\", path_string)) {
      parts <- strsplit(path_string, "\\\\")[[1]]
    }
    
    base_path <- .resolve_keyword(parts[1])
    if (length(parts) > 1) {
      return(do.call(file.path, c(list(base_path), parts[-1])))
    } else {
      return(base_path)
    }
  }
  
  return(.resolve_keyword(path_string))
}

#' Check if Value Contains Special Path Keywords
#' @keywords internal
._qconfig_is_special_keyword <- function(value) {
  if (!is.character(value) || length(value) != 1) {
    return(FALSE)
  }
  
  direct_keywords <- c("home", "cache", "config", "data", "tempdir", "getwd", ".", "..")
  if (value %in% direct_keywords) {
    return(TRUE)
  }
  
  if (grepl("^(home|cache|config|data|tempdir|getwd|\\.|\\.\\.)[|/\\\\]", value)) {
    return(TRUE)
  }
  
  return(FALSE)
}

#' Process Path Input
#' @keywords internal
._qconfig_process_path_input <- function(path_input, allow_create_dir = TRUE) {
  if (is.null(path_input) || nchar(trimws(path_input)) == 0) {
    return(list(path = NULL, success = FALSE, message = "Path cannot be empty"))
  }
  
  cleaned_path <- tryCatch({
    clean_dir_path(path_input, check_exists = FALSE, create_if_missing = FALSE)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(cleaned_path)) {
    return(list(path = NULL, success = FALSE, message = "Invalid path format"))
  }
  
  if (!dir.exists(cleaned_path)) {
    if (allow_create_dir) {
      .icy_text("")
      .icy_alert(paste0("Path does not exist: ", cleaned_path))
      options <- c(
        "Yes, create directory",
        "No, try different path (or Enter)"
      )
      .icy_text(.apply_color("Select an option:", color = "brown"))
      .icy_bullets(options, bullet = "1:")
      .icy_text(paste0("Enter your choice: ", .apply_color("(1-2, or press Enter to skip)", color = "gray")))
      
      repeat {
        user_choice <- readline()
        
        if (nchar(trimws(user_choice)) == 0) {
          return(list(path = NULL, success = FALSE, message = ""))
        }
        
        choice <- suppressWarnings(as.integer(user_choice))
        
        if (!is.na(choice) && choice == 1) {
          dir_created <- tryCatch({
            dir.create(cleaned_path, recursive = TRUE, showWarnings = FALSE)
          }, error = function(e) {
            FALSE
          })
          
          if (dir_created) {
            .icy_success(paste0("Created directory: ", cleaned_path))
            return(list(path = cleaned_path, success = TRUE, message = NULL))
          } else {
            .icy_alert(paste0("Could not create directory: ", cleaned_path))
            return(list(path = NULL, success = FALSE, message = ""))
          }
        } else if (!is.na(choice) && choice == 2) {
          return(list(path = NULL, success = FALSE, message = ""))
        } else {
          .icy_alert("Invalid selection. Please enter 1 or 2.")
        }
      }
    } else {
      return(list(path = NULL, success = FALSE, message = "Path does not exist"))
    }
  }
  
  return(list(path = cleaned_path, success = TRUE, message = NULL))
}

#' Write Configuration Value
#' @keywords internal
._qconfig_write_config_value <- function(var_name, value, write, package, section, verbose, type = NULL, fn_tmpl = NULL, fn_local = NULL) {
  tryCatch({
    switch(write,
      "local" = {
        config_list <- list()
        converted_value <- ._qconfig_convert_by_type(value, type)
        config_list[[var_name]] <- converted_value
        write_local(var_list = config_list, package = package, section = section, fn_tmpl = fn_tmpl, fn_local = fn_local)
        if (verbose) {
          .icy_success(paste0("Written ", var_name, " to local config"))
        }
        TRUE
      },
      "renviron" = {
        config_list <- list()
        config_list[[var_name]] <- as.character(value)
        write_renviron(var_list = config_list)
        if (verbose) {
          .icy_success(paste0("Written ", var_name, " to ~/.Renviron"))
        }
        TRUE
      },
      "session" = {
        do.call(Sys.setenv, stats::setNames(list(as.character(value)), var_name))
        if (verbose) {
          .icy_success(paste0("Set ", var_name, " in current session"))
        }
        TRUE
      }
    )
  }, error = function(e) {
    .icy_warn(paste0("Failed to write ", var_name, ": ", e$message))
    FALSE
  })
}

#' Format Success Message
#' @keywords internal
._qconfig_format_success_message <- function(var_name, value, write) {
  location_display <- switch(write,
    "local" = "local config",
    "renviron" = ".Renviron", 
    "session" = "session",
    write
  )
  
  paste0("Set ", var_name, " to ", value, " in ", location_display)
}

#' Perform Interactive Configuration
#' @keywords internal  
._qconfig_do_interactive_config <- function(var_name, description, options, allow_skip,
                                   note, write, package, section, verbose,
                                   type, allow_custom, allow_create_dir, resolve_paths, fn_tmpl = NULL, fn_local = NULL) {
  
  # Display description if available
  if (!is.null(description) && nchar(description) > 0) {
    wrapped_description <- description
    .icy_title("Description", auto_number = FALSE, level_adjust = -1)
    .icy_text(wrapped_description)
  }
  
  # Display current value if available
  current_value <- tryCatch({
    config <- get_config(package = package, section = section, fn_local = fn_local)
    config[[var_name]]
  }, error = function(e) NULL)
  
  if (!is.null(current_value)) {
    .icy_text("")
    current_display <- if (is.logical(current_value)) {
      toupper(as.character(current_value))
    } else {
      as.character(current_value)
    }
    .icy_text(paste0("Current value: ", .apply_color(current_display, color = "cyan")))
  }
  
  # Display type information if available
  if (!is.null(type) && nchar(type) > 0) {
    type_display <- switch(type,
      "character" = "Text string",
      "integer" = "Integer number",
      "numeric" = "Numeric value",
      "logical" = "Boolean (TRUE/FALSE)",
      "path" = "Directory path",
      type
    )
    .icy_text("")
    .icy_text(paste0("Type: ", .apply_color(type_display, color = "yellow")))
  }
  
  # Add blank line after description/type section
  if (!is.null(description) || !is.null(type)) {
    .icy_text("")
  }
  
  # Display note if provided
  if (!is.null(note) && nchar(note) > 0) {
    .icy_text(paste0("Note: ", note))
    .icy_text("")
  }
  
  # Title
  .icy_title("Selection", auto_number = FALSE, level_adjust = -1)

  # Get user input based on whether options are available
  if (is.null(options) || length(options) == 0) {
    # Manual text input case
    is_manual_input <- TRUE
    
    # Input validation loop for manual text input
    repeat {
      if (allow_skip) {
        prompt_reminder <- "(or press Enter to keep current config)"
      } else {
        prompt_reminder <- ""
      }
      prompt_text <- paste0("Enter value: ", .apply_color(prompt_reminder, color = "gray"))
      
      .icy_text(prompt_text)
      user_input <- readline()
      
      # Handle skip case for manual input
      if (is.null(._qconfig_handle_skip_input(user_input, allow_skip))) return(NULL)
      
      # Validate input for manual entry
      if (!allow_skip && nchar(user_input) == 0) {
        .icy_stop("A value is required")
      }
      
      # Validate type if specified
      validation_error <- ._qconfig_validate_input_type(user_input, type)
      if (!is.null(validation_error)) {
        .icy_alert(validation_error)
        next
      }
      
      break
    }
    
    selected_value <- user_input
    success_msg <- ._qconfig_format_success_message(var_name, user_input, write)
    
  } else {
    # Options selection case
    is_manual_input <- FALSE
    
    if (allow_custom) {
      .icy_text(.apply_color("Select an option or enter custom value:", color = "brown"))
    } else {
      .icy_text(.apply_color("Select an option:", color = "brown"))
    }
    
    # Format options with current value indicator and resolved paths
    formatted_options <- character(length(options))
    for (i in seq_along(options)) {
      option_value <- options[i]
      
      # For path types, resolve special paths for display
      display_value <- if (!is.null(type) && type == "path") {
        resolved <- ._qconfig_resolve_special_path(option_value, package)
        
        # Also resolve relative paths to absolute paths for display
        absolute_path <- tryCatch({
          normalizePath(resolved, mustWork = FALSE)
        }, error = function(e) resolved)
        
        # Show absolute path with original in parentheses if they differ
        if (absolute_path != option_value) {
          paste0(absolute_path, .apply_color(paste0(" (", option_value, ")"), "gray"))
        } else {
          absolute_path
        }
      } else {
        option_value
      }
      
      # Check if this option matches the current value
      if (!is.null(current_value)) {
        # Handle different value types for comparison
        current_display <- if (is.logical(current_value)) {
          toupper(as.character(current_value))
        } else {
          as.character(current_value)
        }
        
        if (option_value == current_display) {
          formatted_options[i] <- paste0(display_value, " ", .apply_color("(current value)", "grey"))
        } else {
          formatted_options[i] <- display_value
        }
      } else {
        formatted_options[i] <- display_value
      }
    }
    
    .icy_bullets(formatted_options, bullet = "1:")
    
    # Add helpful tip for path types with suffix support
    if (type == "path" && resolve_paths == "ask") {
      # Check if any options contain keywords
      keyword_indices <- which(sapply(options, ._qconfig_is_special_keyword))
      if (length(keyword_indices) > 0) {
        # Use the first keyword option number for examples
        example_num <- keyword_indices[1]
        example_keyword <- options[example_num]
        example_resolved <- ._qconfig_resolve_special_path(example_keyword, package)
        
        # Make resolved path absolute for display
        example_absolute <- tryCatch({
          normalizePath(example_resolved, mustWork = FALSE)
        }, error = function(e) example_resolved)
        
        .icy_text("")
        .icy_text(.apply_color("Tip: Add 's' for static or 'd' for dynamic resolution:", "gray"))
        .icy_text(.apply_color(paste0("  - '", example_num, "s' = store ", example_absolute, " (this exact path)"), "gray"))
        .icy_text(.apply_color(paste0("  - '", example_num, "d' = store '", example_keyword, "' (resolve at runtime, adjusts to system)"), "gray"))
      }
    }
    .icy_text("")
    
    # Get user selection with retry loop (simplified version for space)
    repeat {
      # Prompt text logic
      if (allow_custom && allow_skip) {
        prompt_reminder <- paste0("(1-", length(options), ", 'c' for custom, or Enter to keep current config)")
      } else if (allow_custom && !allow_skip) {
        prompt_reminder <- paste0("(1-", length(options), " or 'c' for custom)")
      } else if (!allow_custom && allow_skip) {
        prompt_reminder <- paste0("(1-", length(options), " or Enter to keep current config)")
      } else {
        prompt_reminder <- paste0("(1-", length(options), ")")
      }
      
      .icy_text(paste0("Enter your choice: ", .apply_color(prompt_reminder, color = "gray")))
      user_input <- readline()
      
      # Handle skip case
      if (is.null(._qconfig_handle_skip_input(user_input, allow_skip))) return(NULL)
      
      # Handle custom input
      if (allow_custom && tolower(trimws(user_input)) == "c") {
        if (type == "path") {
          repeat {
            .icy_text("Enter custom directory path:")
            custom_input <- readline()
            if (is.null(._qconfig_handle_skip_input(custom_input, allow_skip))) return(NULL)
            if (nchar(trimws(custom_input)) == 0) {
              .icy_alert("Path cannot be empty")
              next
            }
            resolved_custom_input <- ._qconfig_resolve_special_path(custom_input, package)
            path_result <- ._qconfig_process_path_input(resolved_custom_input, allow_create_dir = allow_create_dir)
            if (path_result$success) {
              selected_value <- path_result$path
              success_msg <- ._qconfig_format_success_message(var_name, selected_value, write)
              break
            } else {
              if (!is.null(path_result$message) && nchar(path_result$message) > 0) {
                .icy_alert(path_result$message)
              }
            }
          }
        } else {
          repeat {
            .icy_text("Enter custom value:")
            custom_input <- readline()
            if (is.null(._qconfig_handle_skip_input(custom_input, allow_skip))) return(NULL)
            if (!allow_skip && nchar(trimws(custom_input)) == 0) {
              .icy_stop("A value is required")
            }
            validation_error <- ._qconfig_validate_input_type(custom_input, type)
            if (!is.null(validation_error)) {
              .icy_alert(validation_error)
              next
            }
            break
          }
          selected_value <- custom_input
          success_msg <- ._qconfig_format_success_message(var_name, selected_value, write)
        }
        break
      }
      
      # Parse selection
      parsed_input <- ._qconfig_parse_selection_input(user_input)
      selection <- parsed_input$selection
      
      if (!is.na(selection) && selection >= 1 && selection <= length(options)) {
        selected_value <- options[selection]
        
        # Check if selected value is current value
        is_current_value <- FALSE
        if (!is.null(current_value)) {
          current_display <- if (is.logical(current_value)) {
            toupper(as.character(current_value))
          } else {
            as.character(current_value)
          }
          is_current_value <- (selected_value == current_display)
        }
        
        if (is_current_value) {
          success_msg <- paste0("Kept current value for ", var_name, ": ", selected_value)
          .icy_success(success_msg)
          return(selected_value)
        }
        
        # For path types, handle validation
        if (!is.null(type) && type == "path") {
          resolved_path <- ._qconfig_resolve_special_path(selected_value, package)
          path_result <- ._qconfig_process_path_input(resolved_path, allow_create_dir = allow_create_dir)
          if (!path_result$success) {
            if (!is.null(path_result$message) && nchar(path_result$message) > 0) {
              .icy_alert(path_result$message)
            }
            next
          }
          selected_value <- path_result$path
        }
        
        success_msg <- ._qconfig_format_success_message(var_name, selected_value, write)
        break
      } else {
        if (allow_custom) {
          .icy_alert(paste0("Invalid selection. Please enter a number (1-", length(options), ") or 'c' for custom."))
        } else {
          .icy_alert("Invalid selection. Please try again.")
        }
      }
    }
  }
  
  # Apply path processing for manual input
  if (!is.null(type) && type == "path" && is_manual_input) {
    resolved_manual_input <- ._qconfig_resolve_special_path(selected_value, package)
    path_result <- ._qconfig_process_path_input(resolved_manual_input, allow_create_dir = allow_create_dir)
    if (!path_result$success) {
      .icy_stop(paste0("Manual path input is invalid: ", path_result$message))
    }
    selected_value <- path_result$path
    success_msg <- ._qconfig_format_success_message(var_name, selected_value, write)
  }
  
  # Write and return
  success <- ._qconfig_write_config_value(var_name, selected_value, write, package, section, verbose, type, fn_tmpl, fn_local)
  if (!success) {
    .icy_stop("Failed to write configuration")
  }
  
  .icy_success(success_msg)
  return(selected_value)
}
