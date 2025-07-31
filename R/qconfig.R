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
#'   Possible values: "character", "integer", "numeric", "logical".
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
#' }
#' @export
qconfig <- function(var_name, package = get_package_name(), user = "default",
                    description = NULL, options = NULL, allow_skip = TRUE, 
                    note = NULL, arg_only = FALSE, write = "local", type = NULL, verbose = FALSE) {
  
  # Display section header
  # .icy_title(var_name)

  # .icy_title("test second title same level")
  # Validate and normalize parameters
  params <- .validate_and_normalize_qconfig_params(
    var_name, package, user, description, options, allow_skip, 
    note, arg_only, write, type, verbose
  )
  
  # Read template data using modular functions 
  template_description <- .get_description(params$var_name, params$package)
  template_type <- .get_type(params$var_name, params$package)  # Already normalized boolean→logical
  template_options <- .get_options(params$var_name, params$package)
  
  # Determine final values (argument > template > none)
  final_description <- if (!is.null(params$description)) params$description else template_description
  final_type <- if (!is.null(params$type)) params$type else template_type
  
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
  
  # Perform interactive configuration (pass final_type for display)
  raw_result <- .do_interactive_config(params$var_name, final_description, final_options, 
                                       params$allow_skip, params$note, params$write, 
                                       params$package, params$user, params$verbose, final_type)
  
  # Convert to proper type and return
  return(.convert_return_value(raw_result, final_type))
}

#' @keywords internal
.validate_and_normalize_qconfig_params <- function(var_name, package, user, description, options, allow_skip, note, arg_only, write, type, verbose) {
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
  
  if (!is.null(type) && (!is.character(type) || length(type) != 1 || !type %in% c("character", "integer", "numeric", "logical", "boolean"))) {
    .icy_stop("type must be NULL or one of: 'character', 'integer', 'numeric', 'logical', 'boolean'")
  }
  
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    .icy_stop("verbose must be TRUE or FALSE")
  }
  
  # Normalize type parameter (boolean logic moved to main function)
  if (!is.null(type) && type %in% c("boolean", "bool")) {
    type <- "logical"
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
    verbose = verbose
  ))
}

#' Get Description for Variable from Template
#'
#' Internal helper function to get shortened description for a variable from template.
#' Returns only the first sentence to keep descriptions concise.
#'
#' @param var_name Variable name
#' @param package Package name
#' @return Character string with shortened description, or NULL if not found
#' @keywords internal
.get_description <- function(var_name, package) {
  tryCatch({
    descriptions_config <- get_config(package = package, origin = "template", user = "descriptions")
    
    if (!is.null(descriptions_config) && var_name %in% names(descriptions_config)) {
      full_description <- descriptions_config[[var_name]]
      
      # Split on ". " and take first sentence only
      first_sentence <- strsplit(full_description, "\\. ", fixed = FALSE)[[1]][1]
      
      # Add period back if it was removed by splitting
      if (!grepl("\\.$", first_sentence)) {
        first_sentence <- paste0(first_sentence, ".")
      }
      
      return(first_sentence)
    }
    
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

#' Get Type for Variable from Template
#'
#' Internal helper function to get type for a variable from template.
#' Handles boolean → logical normalization.
#'
#' @param var_name Variable name
#' @param package Package name
#' @return Character string with normalized type, or NULL if not found
#' @keywords internal
.get_type <- function(var_name, package) {
  tryCatch({
    types_config <- get_config(package = package, origin = "template", user = "types")
    
    if (!is.null(types_config) && var_name %in% names(types_config)) {
      type <- types_config[[var_name]]
      
      # Normalize boolean types to logical
      if (!is.null(type) && type %in% c("boolean", "bool")) {
        type <- "logical"
      }
      
      return(type)
    }
    
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

#' Get Options for Variable from Template
#'
#' Internal helper function to get options for a variable from template.
#'
#' @param var_name Variable name
#' @param package Package name
#' @return Character vector with options, or NULL if not found
#' @keywords internal
.get_options <- function(var_name, package) {
  tryCatch({
    options_config <- get_config(package = package, origin = "template", user = "options")
    
    if (!is.null(options_config) && var_name %in% names(options_config)) {
      return(as.character(options_config[[var_name]]))
    }
    
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
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

#' Perform Interactive Configuration
#'
#' Internal helper function to handle the interactive configuration process.
#'
#' @param var_name Variable name
#' @param description Description text
#' @param options Option values
#' @param allow_skip Allow skip flag
#' @param note Note text
#' @param write Write destination
#' @param package Package name
#' @param user User section
#' @param verbose Verbose flag
#' @param type Variable type
#' @return Raw result value or NULL if skipped
#' @keywords internal
.do_interactive_config <- function(var_name,
                                   description,
                                   options, allow_skip,
                                   note, write, package,
                                   user, verbose,
                                   type) {
  
  # Display description if available
  if (!is.null(description) && nchar(description) > 0) {
    wrapped_description <- description
    .icy_title("Description", auto_number = FALSE)
    .icy_text(wrapped_description)
  }
  
  # Display type information if available
  if (!is.null(type) && nchar(type) > 0) {
    type_display <- switch(type,
      "character" = "Text string",
      "integer" = "Integer number",
      "numeric" = "Numeric value",
      "logical" = "Boolean (TRUE/FALSE)",
      type  # fallback to raw type name
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
  .icy_title("Selection", auto_number = FALSE)

  # Get user input based on whether options are available
  if (is.null(options) || length(options) == 0) {
    # Manual text input case
    if (allow_skip) {
      prompt_reminder <- "(or press Enter to keep current config)"
    } else {
      prompt_reminder <- ""
    }
    prompt_text <- paste0("Enter value: ", .apply_color(prompt_reminder, color = "gray"))
    
    .icy_text(prompt_text)
    user_input <- readline()
    
    # Handle skip case for manual input
    if (is.null(.handle_skip_input(user_input, allow_skip))) return(NULL)
    
    # Validate input for manual entry
    if (!allow_skip && nchar(user_input) == 0) {
      .icy_stop("A value is required")
    }
    
    selected_value <- user_input
    success_msg <- paste0("Set ", var_name, " = ", user_input)
    
  } else {
    # Options selection case
    .icy_text("Select an option:")
    .icy_bullets(options, bullet = "1:")
    .icy_text("")
    
    # Get user selection with retry loop
    repeat {
      if (allow_skip) {
        prompt_reminder <- paste0("(1-", length(options), " or Enter to keep current config)")
      } else {
        prompt_reminder <- paste0("(1-", length(options), ")")
      }
      prompt_text <- paste0("Enter your choice: ", .apply_color(prompt_reminder, color = "gray"))
      
      .icy_text(prompt_text, indentation = FALSE)
      user_input <- readline()
      
      # Handle skip case inside the loop
      if (is.null(.handle_skip_input(user_input, allow_skip))) return(NULL)
      
      # Try to parse selection
      selection <- suppressWarnings(as.integer(user_input))
      
      if (!is.na(selection) && selection >= 1 && selection <= length(options)) {
        selected_value <- options[selection]
        success_msg <- paste0("Selected ", selected_value, " for ", var_name)
        break
      } else {
        .icy_alert("Invalid selection. Please try again.")
      }
    }
  }
  
  # Common write and success handling for both paths
  success <- .write_config_value(var_name, selected_value, write, package, user, verbose, type)
  if (!success) {
    .icy_stop("Failed to write configuration")
  }
  
  .icy_success(success_msg)
  return(selected_value)
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

