#' Update Package Configuration Template
#'
#' Programmatically add, update, or remove variables from existing YAML configuration 
#' template files. This function provides precise control over template modifications
#' while preserving structure and consistency.
#'
#' @description
#' This function allows package developers to modify existing configuration templates by:
#' \itemize{
#'   \item Adding new variables with complete metadata (description, type, options)
#'   \item Updating existing variable properties and values  
#'   \item Removing variables from all template sections and metadata
#'   \item Batch operations for multiple variables
#'   \item Automatic backup creation before modifications
#' }
#'
#' All changes maintain template structure and validate against icy conventions.
#'
#' @param action Character string specifying the operation: "add", "update", or "remove".
#' @param var_name Character string with the variable name to modify. For batch operations,
#'   use a character vector of variable names.
#' @param package Character string with the package name. Defaults to `get_package_name()` 
#'   to detect the calling package.
#' @param default_value Default value for the variable (required for "add" and "update").
#'   Can be a single value or list of values matching var_name length.
#' @param description Character string describing the variable for users (optional).
#'   Required for "add" action. Can be character vector for batch operations.
#' @param type Character string specifying variable type: "character", "logical", 
#'   "integer", "numeric", or "path". If NULL, auto-detected from default_value.
#' @param note Character string with additional notes for the variable (optional).
#' @param options Character vector of valid options for choice variables (optional).
#' @param sections Character vector specifying which sections to modify. 
#'   Use "all" to modify all sections, "default" for default section only,
#'   or specific section names. Default: c("default").
#' @param fn_tmpl Character string with path to the template file. If NULL (default),
#'   searches for the standard template file for the package.
#' @param backup Logical. If TRUE (default), creates a backup file before modifications.
#' @param case_format Character string for template file search if fn_tmpl is NULL.
#'   Options: "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param verbose Logical. If TRUE (default), displays progress and confirmation messages.
#'
#' @return Logical indicating success (invisible). TRUE if operation completed successfully.
#'
#' @examples
#' \dontrun{
#' # Add a new variable
#' update_template(
#'   action = "add",
#'   var_name = "MYPACKAGE_DEBUG",
#'   default_value = FALSE,
#'   description = "Enable debug mode with detailed logging",
#'   type = "logical"
#' )
#'
#' # Update existing variable
#' update_template(
#'   action = "update", 
#'   var_name = "MYPACKAGE_TIMEOUT",
#'   default_value = 60,
#'   description = "Request timeout in seconds (updated default)"
#' )
#'
#' # Remove a variable completely
#' update_template(
#'   action = "remove",
#'   var_name = "MYPACKAGE_OLD_VAR"
#' )
#'
#' # Add variable with options
#' update_template(
#'   action = "add",
#'   var_name = "MYPACKAGE_LOG_LEVEL", 
#'   default_value = "INFO",
#'   description = "Logging level for package operations",
#'   options = c("DEBUG", "INFO", "WARNING", "ERROR")
#' )
#'
#' # Batch operation - add multiple variables
#' update_template(
#'   action = "add",
#'   var_name = c("MYPACKAGE_VAR1", "MYPACKAGE_VAR2"),
#'   default_value = list("value1", TRUE),
#'   description = c("First variable", "Second variable")
#' )
#' }
#'
#' @export
update_template <- function(action = NULL,
                           var_name = NULL,
                           package = get_package_name(verbose = FALSE),
                           default_value = NULL,
                           description = NULL,
                           type = NULL,
                           note = NULL,
                           options = NULL,
                           sections = "default",
                           fn_tmpl = NULL,
                           backup = TRUE,
                           case_format = "snake_case",
                           verbose = TRUE,
                           interactive = FALSE,
                           template_data = NULL) {
  
  # Find template file first (needed for both modes)
  template_path <- find_template(package = package, fn_tmpl = fn_tmpl, case_format = case_format)
  
  if (is.null(template_path)) {
    .icy_stop(paste0("No template configuration file found for package ", package))
  }
  
  # Handle interactive mode
  if (interactive) {
    if (verbose) {
      .icy_text(paste0("Template: ", .apply_color(template_path, "yellow")))
    }
    
    # Read existing template or use provided data
    if (is.null(template_data)) {
      if (!file.exists(template_path)) {
        .icy_stop(paste0("Template file not found: ", template_path))
      }
      template_data <- tryCatch({
        yaml::read_yaml(template_path)
      }, error = function(e) {
        .icy_stop(paste0("Error reading template file: ", e$message))
      })
    }
    
    # Enter interactive mode
    updated_data <- .interactive_template_builder(
      template_data = template_data,
      package = package,
      template_file = template_path,
      verbose = verbose,
      debug = FALSE
    )
    
    # Write final template using unified function
    .write_template_with_header(
      template_data = updated_data,
      file_path = template_path,
      package = package,
      verbose = verbose,
      auto_save = FALSE
    )
    
    return(invisible(TRUE))
  }
  
  # Programmatic mode - validate parameters
  valid_actions <- c("add", "update", "remove")
  
  # Handle NULL or missing action parameter
  if (is.null(action)) {
    if (verbose) {
      .icy_text(.apply_color("No action specified. Entering interactive mode...", color = "gray"))
    }
    # Recursively call with interactive = TRUE
    return(update_template(
      package = package,
      fn_tmpl = fn_tmpl,
      backup = backup,
      case_format = case_format,
      verbose = verbose,
      interactive = TRUE,
      template_data = template_data
    ))
  }
  
  if (!action %in% valid_actions) {
    .icy_stop(c(
      paste0("Invalid action: ", action),
      "i" = paste0("Valid actions are: ", paste(valid_actions, collapse = ", "))
    ))
  }
  
  # Validate var_name
  if (missing(var_name) || is.null(var_name) || length(var_name) == 0) {
    .icy_stop("var_name is required")
  }
  
  if (verbose) {
    .icy_title(paste0("Template Update: ", tools::toTitleCase(action)))
    .icy_text(paste0("Template: ", .apply_color(template_path, "yellow")))
    .icy_text(paste0("Variables: ", paste(.apply_color(var_name, "cyan"), collapse = ", ")))
  }
  
  # Validate inputs for batch operations
  .validate_batch_inputs(action, var_name, default_value, description, type, note)
  
  # Create backup if requested
  if (backup) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_path <- paste0(template_path, ".backup_", timestamp)
    file.copy(template_path, backup_path)
    if (verbose) {
      .icy_text(paste0("Backup created: ", .apply_color(backup_path, "dim")))
    }
  }
  
  # Read existing template
  if (!file.exists(template_path)) {
    .icy_stop(paste0("Template file not found: ", template_path))
  }
  template_data <- tryCatch({
    yaml::read_yaml(template_path)
  }, error = function(e) {
    .icy_stop(paste0("Error reading template file: ", e$message))
  })
  
  # Perform the requested operation
  template_data <- if (action == "remove") {
    .remove_template_variables(template_data, var_name, sections, verbose)
  } else {
    .perform_template_operation(action, template_data, var_name, default_value, 
                               description, type, note, options, sections, verbose)
  }
  
  # Write updated template using unified function
  .write_template_with_header(
    template_data = template_data,
    file_path = template_path,
    package = package,
    verbose = FALSE,  # Success message handled below
    auto_save = FALSE
  )
  
  if (verbose) {
    .icy_success(paste0("Template ", action, " completed successfully"))
  }
  
  return(invisible(TRUE))
}




#' Validate batch operation inputs
#' @keywords internal
.validate_batch_inputs <- function(action, var_name, default_value, description, type, note) {
  n_vars <- length(var_name)
  
  if (action %in% c("add", "update")) {
    # Check default_value
    if (is.null(default_value)) {
      .icy_stop("default_value is required for add and update actions")
    }
    
    if (is.list(default_value)) {
      if (length(default_value) != n_vars) {
        .icy_stop("Length of default_value list must match number of variables")
      }
    } else if (n_vars > 1) {
      .icy_stop("For multiple variables, default_value must be a list")
    }
    
    # Check description for add action
    if (action == "add") {
      if (is.null(description)) {
        .icy_stop("description is required for add action")
      }
      if (length(description) != 1 && length(description) != n_vars) {
        .icy_stop("description must be single value or match number of variables")
      }
    }
    
    # Check other optional parameters
    if (!is.null(type) && length(type) != 1 && length(type) != n_vars) {
      .icy_stop("type must be single value or match number of variables")
    }
    
    if (!is.null(note) && length(note) != 1 && length(note) != n_vars) {
      .icy_stop("note must be single value or match number of variables") 
    }
  }
}
















#' Remove variables from template
#' @keywords internal
.remove_template_variables <- function(template_data, var_names, sections, verbose) {
  
  for (var_name in var_names) {
    removed_from_sections <- character(0)
    
    # Remove from specified sections
    if ("all" %in% sections) {
      # Remove from all data sections (not metadata)
      data_sections <- setdiff(names(template_data), c("descriptions", "types", "notes", "options"))
      target_sections <- data_sections
    } else {
      target_sections <- sections
    }
    
    for (section in target_sections) {
      if (section %in% names(template_data) && var_name %in% names(template_data[[section]])) {
        template_data[[section]][[var_name]] <- NULL
        removed_from_sections <- c(removed_from_sections, section)
      }
    }
    
    # Remove from metadata sections
    metadata_sections <- c("types", "descriptions", "notes", "options", "inheritances")
    for (section in metadata_sections) {
      if (section %in% names(template_data) && var_name %in% names(template_data[[section]])) {
        template_data[[section]][[var_name]] <- NULL
      }
    }
    
    # Clean up empty sections
    template_data <- .clean_yaml_structure(template_data, remove_empty = TRUE)
    
    if (verbose) {
      if (length(removed_from_sections) > 0) {
        .icy_success(paste0("Removed ", .apply_color(var_name, "red"), " from: ", 
                           paste(removed_from_sections, collapse = ", ")))
      } else {
        .icy_alert(paste0("Variable ", var_name, " not found in specified sections"))
      }
    }
  }
  
  return(template_data)
}




