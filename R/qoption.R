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
#' api_key <- qoption("DUMMY_API_KEY", package = "dummy")
#' # Uses template description and options, writes to local YAML config
#'
#' # Write to ~/.Renviron instead
#' port <- qoption("DUMMY_DB_PORT", write = "renviron")
#' # Writes selected value to ~/.Renviron file
#'
#' # Set only in current session
#' log_level <- qoption("DUMMY_LOG_LEVEL", write = "session")
#' # Sets value using Sys.setenv() for current session only
#'
#' # Add custom options to template options
#' timeout <- qoption("DUMMY_TIMEOUT", options = c("30", "60", "120"))
#' # Shows custom options first, then template options, writes to local config
#'
#' # Skip functionality - no writing occurs
#' optional_var <- qoption("DUMMY_OPTIONAL", allow_skip = TRUE)
#' # Can return NULL if user skips, no configuration is written
#' }
#'
#' @export
qoption <- function(var_name, package = get_package_name(), user = "default",
                    description = NULL, options = NULL, allow_skip = TRUE, 
                    note = NULL, arg_only = FALSE, write = "local", type = NULL, verbose = FALSE) {
  
  # Input validation
  if (!is.character(var_name) || length(var_name) != 1 || nchar(var_name) == 0) {
    .icy_abort("var_name must be a non-empty character string")
  }
  
  if (!is.character(package) || length(package) != 1 || nchar(package) == 0) {
    .icy_abort("package must be a non-empty character string")
  }
  
  if (!is.character(user) || length(user) != 1 || nchar(user) == 0) {
    .icy_abort("user must be a non-empty character string")
  }
  
  if (!is.null(description) && (!is.character(description) || length(description) != 1)) {
    .icy_abort("description must be NULL or a character string")
  }
  
  if (!is.null(options) && !is.vector(options)) {
    .icy_abort("options must be NULL or a vector")
  }
  
  if (!is.logical(allow_skip) || length(allow_skip) != 1 || is.na(allow_skip)) {
    .icy_abort("allow_skip must be TRUE or FALSE")
  }
  
  if (!is.null(note) && (!is.character(note) || length(note) != 1)) {
    .icy_abort("note must be NULL or a character string")
  }
  
  if (!is.logical(arg_only) || length(arg_only) != 1 || is.na(arg_only)) {
    .icy_abort("arg_only must be TRUE or FALSE")
  }
  
  if (!is.character(write) || length(write) != 1 || !write %in% c("local", "renviron", "session")) {
    .icy_abort("write must be one of: 'local', 'renviron', 'session'")
  }
  
  if (!is.null(type) && (!is.character(type) || length(type) != 1 || !type %in% c("character", "integer", "numeric", "logical", "boolean"))) {
    .icy_abort("type must be NULL or one of: 'character', 'integer', 'numeric', 'logical', 'boolean'")
  }
  
  # Normalize type parameter
  if (!is.null(type) && type %in% c("boolean", "bool")) {
    type <- "logical"
  }
  
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    .icy_abort("verbose must be TRUE or FALSE")
  }
  
  # Read template configuration to get descriptions, options, and type
  template_description <- NULL
  template_options <- NULL
  template_type <- NULL
  
  template_path <- tryCatch({
    find_template(package = package, verbose = FALSE)
  }, error = function(e) NULL)
  
  if (!is.null(template_path)) {
    yaml_content <- tryCatch({
      yaml::read_yaml(template_path)
    }, error = function(e) NULL)
    
    if (!is.null(yaml_content)) {
      # Get description from template
      if (!is.null(yaml_content[["descriptions"]]) && !is.null(yaml_content[["descriptions"]][[var_name]])) {
        template_description <- yaml_content[["descriptions"]][[var_name]]
      }
      
      # Get options from template
      if (!is.null(yaml_content[["options"]]) && !is.null(yaml_content[["options"]][[var_name]])) {
        template_options <- yaml_content[["options"]][[var_name]]
        # Convert to character vector
        template_options <- as.character(template_options)
      }
      
      # Get type from template
      if (!is.null(yaml_content[["types"]]) && !is.null(yaml_content[["types"]][[var_name]])) {
        template_type <- yaml_content[["types"]][[var_name]]
        # Normalize boolean/logical type names
        if (template_type %in% c("boolean", "bool")) {
          template_type <- "logical"
        }
      }
    }
  }
  
  # Determine final values (argument > template > none)
  final_description <- if (!is.null(description)) description else template_description
  final_type <- if (!is.null(type)) type else template_type
  
  # Determine final options (merge or arg_only)
  final_options <- NULL
  if (!is.null(options)) {
    final_options <- as.character(options)
    if (!arg_only && !is.null(template_options)) {
      # Merge: argument options first, then template options (remove duplicates)
      final_options <- unique(c(final_options, template_options))
    }
  } else if (!is.null(template_options)) {
    final_options <- template_options
  }
  
  # Display variable name as header with color
  .icy_text(paste0("\n", .apply_color(paste0("--- ", var_name, " ---"), "blue", "bold")))
  
  # Display description if available
  if (!is.null(final_description) && nchar(final_description) > 0) {
    # Add "Description: " prefix and wrap text
    wrapped_description <- .wrap_text(paste0("Description: ", final_description), width = 80)
    cat(.apply_color(wrapped_description, "yellow"), "\n\n", sep = "")
  }
  
  # Display note if provided
  if (!is.null(note) && nchar(note) > 0) {
    .icy_alert_info(note)
  }
  
  # Handle options or manual input
  if (!is.null(final_options) && length(final_options) > 0) {
    # Display numbered options with indentation
    .icy_alert_info("Choose an option:")
    for (i in seq_along(final_options)) {
      cat("    ", i, ": ", final_options[i], "\n", sep = "")
    }
    
    # Prepare prompt
    skip_text <- if (allow_skip) .apply_color(" or press Enter to skip", "grey") else ""
    prompt_text <- paste0("Enter choice (1-", length(final_options), "):", skip_text)
    
    repeat {
      .icy_text(prompt_text)
      answer <- readline("> ")
      
      # Handle empty input (skip if allowed)
      if (nchar(trimws(answer)) == 0) {
        if (allow_skip) {
          .icy_alert_info(paste0("Skipped configuration for ", var_name))
          return(invisible(NULL))
        } else {
          .icy_alert_warning("Please select an option.")
          next
        }
      }
      
      # Try to parse as integer
      choice <- suppressWarnings(as.integer(trimws(answer)))
      
      # Validate choice
      if (is.na(choice)) {
        .icy_alert_warning(paste0("Invalid input: '", answer, "'. Please enter a number between 1 and ", length(final_options), "."))
        next
      }
      
      if (choice < 1 || choice > length(final_options)) {
        .icy_alert_warning(paste0("Choice out of range: ", choice, ". Please enter a number between 1 and ", length(final_options), "."))
        next
      }
      
      # Valid choice
      selected_value <- final_options[choice]
      .icy_alert_success(paste0("Selected: ", .apply_color(selected_value, "green", "bold")))
      
      # Write the configuration
      write_success <- .write_config_value(var_name, selected_value, write, package, user, verbose, final_type)
      
      return(invisible(selected_value))
    }
  } else {
    # No options available - prompt for manual input
    .icy_alert_info("Enter value manually:")
    skip_text <- if (allow_skip) .apply_color(" (or press Enter to skip)", "grey") else ""
    prompt_text <- paste0("Value for ", var_name, skip_text, ":")
    
    repeat {
      .icy_text(prompt_text)
      answer <- readline("> ")
      
      # Handle empty input
      if (nchar(trimws(answer)) == 0) {
        if (allow_skip) {
          .icy_alert_info(paste0("Skipped configuration for ", var_name))
          return(invisible(NULL))
        } else {
          .icy_alert_warning("Please enter a value.")
          next
        }
      }
      
      # Return the entered value
      entered_value <- trimws(answer)
      .icy_alert_success(paste0("Entered value: ", .apply_color(entered_value, "green", "bold")))
      
      # Write the configuration
      write_success <- .write_config_value(var_name, entered_value, write, package, user, verbose, final_type)
      
      return(invisible(entered_value))
    }
  }
}

#' Wrap Text to Specified Width
#'
#' Internal helper function to wrap text to a specified width, respecting word boundaries.
#'
#' @param text Text to wrap
#' @param width Maximum width (defaults to 120, but adjusts to terminal width if smaller)
#'
#' @return Wrapped text string
#' @keywords internal
.wrap_text <- function(text, width = 120) {
  # Get terminal width if available, otherwise use default
  terminal_width <- tryCatch({
    if (interactive() && requireNamespace("cli", quietly = TRUE)) {
      cli::console_width()
    } else {
      getOption("width", 80)
    }
  }, error = function(e) 80)
  
  # Use smaller of requested width or terminal width (with some margin)
  effective_width <- min(width, terminal_width - 4)
  
  # Simple word wrapping
  if (nchar(text) <= effective_width) {
    return(text)
  }
  
  words <- strsplit(text, " ")[[1]]
  lines <- character(0)
  current_line <- ""
  
  for (word in words) {
    test_line <- if (current_line == "") word else paste(current_line, word)
    
    if (nchar(test_line) <= effective_width) {
      current_line <- test_line
    } else {
      if (current_line != "") {
        lines <- c(lines, current_line)
      }
      current_line <- word
    }
  }
  
  if (current_line != "") {
    lines <- c(lines, current_line)
  }
  
  return(paste(lines, collapse = "\n"))
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
          .icy_alert_success(paste0("Written ", var_name, " to local config"))
        }
        TRUE
      },
      "renviron" = {
        config_list <- list()
        # .Renviron always stores as strings, so use original value
        config_list[[var_name]] <- as.character(value)
        write_renviron(var_list = config_list)
        if (verbose) {
          .icy_alert_success(paste0("Written ", var_name, " to ~/.Renviron"))
        }
        TRUE
      },
      "session" = {
        # Sys.setenv always stores as strings, so use original value
        do.call(Sys.setenv, setNames(list(as.character(value)), var_name))
        if (verbose) {
          .icy_alert_success(paste0("Set ", var_name, " in current session"))
        }
        TRUE
      }
    )
  }, error = function(e) {
    .icy_alert_warning(paste0("Failed to write ", var_name, ": ", e$message))
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
  # If no type specified, handle special boolean case only
  if (is.null(type)) {
    if (value == "yes") return(TRUE)
    if (value == "no") return(FALSE)
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

