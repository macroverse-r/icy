#' Resolve Special Path Keywords
#'
#' Internal function to resolve special keywords and path combinations in paths.
#' Handles keywords like "home", "cache", "config", "data", "tempdir", "getwd", ".", ".."
#' and path combinations like "home|Documents", "cache|logs", ".|data".
#'
#' @param path_string Character string containing path with potential keywords or combinations
#' @param package Character string with package name for package-specific cache directories
#' @return Resolved path string with platform-appropriate paths
#' @keywords internal
.resolve_special_path <- function(path_string, package = NULL) {
  # Helper function to resolve keywords
  .resolve_keyword <- function(keyword) {
    switch(keyword,
      "home" = path.expand("~"),
      "cache" = tools::R_user_dir(package, "cache"),
      "config" = tools::R_user_dir(package, "config"), 
      "data" = tools::R_user_dir(package, "data"),
      "tempdir" = tempdir(),
      "getwd" = getwd(),
      "." = getwd(),           # Current directory (same as getwd)
      ".." = dirname(getwd()), # Parent directory
      keyword  # Return unchanged if not a special keyword
    )
  }
  
  # Handle path combinations (e.g., "home|Documents", "home/Documents", "home\\Documents") 
  if (grepl("[|/\\\\]", path_string)) {
    # Determine which separator is used and split accordingly
    if (grepl("\\|", path_string)) {
      parts <- strsplit(path_string, "\\|")[[1]]
    } else if (grepl("/", path_string)) {
      parts <- strsplit(path_string, "/")[[1]]
    } else if (grepl("\\\\", path_string)) {
      parts <- strsplit(path_string, "\\\\")[[1]]
    }
    
    base_path <- .resolve_keyword(parts[1])
    # Join with remaining parts
    if (length(parts) > 1) {
      return(do.call(file.path, c(list(base_path), parts[-1])))
    } else {
      return(base_path)
    }
  }
  
  # Handle single keywords
  return(.resolve_keyword(path_string))
}

#' Validate and Normalize qconfig Parameters
#'
#' Internal helper function to validate all qconfig parameters and normalize types.
#'
#' @param var_name Variable name
#' @param package Package name
#' @param user User section
#' @param description Description text
#' @param options Option values
#' @param allow_skip Allow skip flag
#' @param note Note text
#' @param arg_only Argument only flag
#' @param write Write destination
#' @param type Variable type
#' @param allow_custom Allow custom input flag
#' @param allow_create_dir Allow directory creation flag
#' @param resolve_paths Path resolution mode
#' @param verbose Verbose flag
#' @return List of validated and normalized parameters
#' @keywords internal
.validate_and_normalize_qconfig_params <- function(var_name, package, user, description, options, allow_skip, note, arg_only, write, type, allow_custom, allow_create_dir, resolve_paths, verbose) {
  # Input validation
  if (!is.character(var_name) || length(var_name) != 1 || nchar(var_name) == 0) {
    .icy_stop("var_name must be a non-empty character string")
  }
  
  if (!is.character(package) || length(package) != 1 || nchar(package) == 0) {
    .icy_stop("package must be a non-empty character string")
  }
  
  if (!is.character(user) || length(user) != 1 || nchar(user) == 0) {
    .icy_stop("user must be a non-empty character string")
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
  
  # Normalize type parameter (boolean logic moved to main function)
  if (!is.null(type) && type %in% c("boolean", "bool")) {
    type <- "logical"
  }
  if (!is.null(type) && type == "dir") {
    type <- "path"
  }
  
  return(list(
    var_name = var_name,
    package = package,
    user = user,
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
    verbose = verbose
  ))
}

#' Convert Return Value to Proper Type
#'
#' Internal helper function to convert string values to proper R types.
#'
#' @param value Raw value (character string or NULL)
#' @param type Expected type
#' @return Converted value with proper R type
#' @keywords internal
.convert_return_value <- function(value, type) {
  # Return NULL as-is
  if (is.null(value)) {
    return(invisible(NULL))
  }
  
  # Convert based on type
  if (is.null(type)) {
    return(invisible(value))  # No type specified, return as-is
  }
  
  converted_value <- switch(type,
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
      if (value %in% c("yes", "true", "TRUE", "True", "on", "1")) {
        TRUE
      } else if (value %in% c("no", "false", "FALSE", "False", "off", "0")) {
        FALSE
      } else {
        value  # Return original if can't convert
      }
    },
    # Default: return as-is
    value
  )
  
  return(invisible(converted_value))
}

#' Write Configuration Value
#'
#' Internal helper function to write configuration values to the specified location.
#'
#' @param var_name Variable name
#' @param value Value to write
#' @param write Write location ("local", "renviron", "session")
#' @param package Package name 
#' @param user User section
#' @param verbose Whether to show confirmation messages
#' @param type Expected type for the variable (NULL means no conversion)
#'
#' @return TRUE if successful, FALSE otherwise
#' @keywords internal
.write_config_value <- function(var_name, value, write, package, user, verbose, type = NULL) {
  tryCatch({
    switch(write,
      "local" = {
        config_list <- list()
        # Convert value based on type information
        converted_value <- .convert_by_type(value, type)
        config_list[[var_name]] <- converted_value
        write_local(var_list = config_list, package = package, user = user)
        if (verbose) {
          .icy_success(paste0("Written ", var_name, " to local config"))
        }
        TRUE
      },
      "renviron" = {
        config_list <- list()
        # .Renviron always stores as strings, so use original value
        config_list[[var_name]] <- as.character(value)
        write_renviron(var_list = config_list)
        if (verbose) {
          .icy_success(paste0("Written ", var_name, " to ~/.Renviron"))
        }
        TRUE
      },
      "session" = {
        # Sys.setenv always stores as strings, so use original value
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

#' Convert Value by Type
#'
#' Converts a string value to the specified type for proper YAML representation.
#'
#' @param value Value to convert
#' @param type Target type ("character", "integer", "numeric", "logical", or NULL)
#' @return Converted value
#' @keywords internal
.convert_by_type <- function(value, type) {
  # Handle NULL type - return value as-is
  if (is.null(type)) {
    return(value)
  }
  
  # Convert based on specified type
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
      if (value %in% c("yes", "true", "TRUE", "True", "on", "1")) {
        TRUE
      } else if (value %in% c("no", "false", "FALSE", "False", "off", "0")) {
        FALSE
      } else {
        value  # Return original if can't convert
      }
    },
    # Default: return as-is
    value
  )
}

#' Handle Skip Input
#'
#' Internal helper function to handle skip logic consistently.
#'
#' @param user_input User input string
#' @param allow_skip Whether skipping is allowed
#' @return NULL if skipped, FALSE if not skipped
#' @keywords internal
.handle_skip_input <- function(user_input, allow_skip) {
  if (allow_skip && nchar(user_input) == 0) {
    .icy_inform("Skipped configuration")
    return(NULL)
  }
  return(FALSE)  # Not skipped
}

#' Format Success Message
#'
#' Internal helper function to format consistent success messages.
#'
#' @param var_name Variable name
#' @param value Selected or entered value
#' @param write Write location ("local", "renviron", "session")
#' @return Formatted success message
#' @keywords internal
.format_success_message <- function(var_name, value, write) {
  # Map write locations to display names
  location_display <- switch(write,
    "local" = "local config",
    "renviron" = ".Renviron", 
    "session" = "session",
    write  # fallback
  )
  
  paste0("Set ", var_name, " to ", value, " in ", location_display)
}

#' Process Path Input
#'
#' Internal helper function to clean and validate directory path input with interactive directory creation.
#'
#' @param path_input Character string with user-provided path input
#' @param allow_create_dir Logical; whether to allow interactive directory creation if missing
#' @return List with cleaned path, success status, and error message
#' @keywords internal
.process_path_input <- function(path_input, allow_create_dir = TRUE) {
  # Handle empty input
  if (is.null(path_input) || nchar(trimws(path_input)) == 0) {
    return(list(path = NULL, success = FALSE, message = "Path cannot be empty"))
  }
  
  # Clean path format first (without existence check)
  cleaned_path <- tryCatch({
    clean_dir_path(path_input, check_exists = FALSE, create_if_missing = FALSE)
  }, error = function(e) {
    return(NULL)
  })
  
  # Handle invalid path format
  if (is.null(cleaned_path)) {
    return(list(path = NULL, success = FALSE, message = "Invalid path format"))
  }
  
  # Check if directory exists
  if (!dir.exists(cleaned_path)) {
    if (allow_create_dir) {
      # Interactive prompt for directory creation
      .icy_text("")
      .icy_alert(paste0("Path does not exist: ", cleaned_path))
      .icy_text("Do you want to create it?")
      .icy_text("  1: Yes, create directory")
      .icy_text("  0: No, try different path (or Enter)")
      .icy_text("")
      
      repeat {
        .icy_text("Enter your choice: (0-1)", indentation = FALSE)
        user_choice <- readline()
        
        # Handle empty input (default to 0/no)
        if (nchar(trimws(user_choice)) == 0) {
          return(list(path = NULL, success = FALSE, message = ""))
        }
        
        # Parse choice
        choice <- suppressWarnings(as.integer(user_choice))
        
        if (!is.na(choice) && choice == 1) {
          # Attempt to create directory
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
        } else if (!is.na(choice) && choice == 0) {
          # User chose not to create
          return(list(path = NULL, success = FALSE, message = ""))
        } else {
          # Invalid selection
          .icy_alert("Invalid selection. Please enter 0 or 1.")
        }
      }
    } else {
      # No creation allowed
      return(list(path = NULL, success = FALSE, message = "Path does not exist"))
    }
  }
  
  # Path exists, return success
  return(list(path = cleaned_path, success = TRUE, message = NULL))
}

#' Check if Value Contains Special Path Keywords
#'
#' Internal helper function to detect if a value contains special path keywords
#' that can be resolved dynamically at runtime.
#'
#' @param value Character string to check for keywords
#' @return Logical indicating whether the value contains special keywords
#' @keywords internal
.is_special_keyword <- function(value) {
  if (!is.character(value) || length(value) != 1) {
    return(FALSE)
  }
  
  # Direct keywords
  direct_keywords <- c("home", "cache", "config", "data", "tempdir", "getwd", ".", "..")
  if (value %in% direct_keywords) {
    return(TRUE)
  }
  
  # Path combinations with keywords (e.g., "home|Documents", "cache/logs")
  if (grepl("^(home|cache|config|data|tempdir|getwd|\\.|\\.\\.)[|/\\\\]", value)) {
    return(TRUE)
  }
  
  return(FALSE)
}

#' Parse Selection Input with Resolution Mode Suffixes
#'
#' Internal helper function to parse user input that may contain resolution
#' mode suffixes (s for static, d for dynamic).
#'
#' @param input Character string with user input (e.g., "8", "7s", "3d")
#' @return List with selection number and resolution mode
#' @keywords internal
.parse_selection_input <- function(input) {
  input <- trimws(input)
  
  # Check for suffix patterns (number followed by s or d)
  if (grepl("^[0-9]+[sd]$", input)) {
    number <- as.integer(gsub("[sd]$", "", input))
    mode <- if (endsWith(input, "s")) "static" else "dynamic"
    return(list(selection = number, mode = mode))
  }
  
  # Regular selection number - will ask if needed
  number <- suppressWarnings(as.integer(input))
  return(list(selection = number, mode = "ask"))
}

#' Determine Allow Custom Setting
#'
#' Internal helper function to determine if custom input should be allowed
#' based on variable type when allow_custom is NULL.
#'
#' @param type Variable type (e.g., "logical", "path", "character", etc.)
#' @param allow_custom User-specified allow_custom parameter
#' @return Logical value indicating whether custom input should be allowed
#' @keywords internal
.determine_allow_custom <- function(type, allow_custom) {
  # If explicitly set, use that value
  if (!is.null(allow_custom)) {
    return(allow_custom)
  }
  
  # Smart defaults based on type
  if (is.null(type)) {
    return(TRUE)  # Default when no type information
  }
  
  switch(type,
    "logical" = FALSE,  # Boolean values should be TRUE/FALSE only
    "path" = TRUE,      # Users often need custom directory paths  
    "character" = TRUE, # Users may need custom text input
    "integer" = TRUE,   # Users may need custom numeric values
    "numeric" = TRUE,   # Users may need custom numeric values
    TRUE                # Default to allowing custom input
  )
}