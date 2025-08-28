#' Common Template Operations
#'
#' Shared utilities for template creation and modification operations.
#' These functions are used by both create_template() and update_template()
#' to eliminate code duplication and maintain consistency.
#'
#' @name template-common
#' @keywords internal
NULL

#' Collect Variable Information Interactively
#'
#' Interactive function to collect variable name, description, type, default value,
#' notes, and options from user input. This is the core interactive logic shared
#' between create_template() and update_template().
#'
#' @param package Package name for variable prefix suggestions
#' @param existing_vars Character vector of existing variable names to check for duplicates
#' @return List with variable information or NULL if cancelled
#' @keywords internal
.collect_variable_interactively <- function(package, existing_vars = character(0)) {
  package_upper <- toupper(package)
  
  # Get variable name
  .icy_text("")
  .icy_text(paste0("Variable name ", .apply_color("(e.g., API_KEY, DEBUG_MODE)", "gray"), ":"))
  var_name <- trimws(readline())
  
  if (nchar(var_name) == 0) {
    return(NULL)
  }
  
  # Validate and clean name
  name_validation <- .validate_variable_name(
    var_name,
    package = package,
    existing_vars = existing_vars,
    require_prefix = FALSE,
    auto_clean = TRUE
  )
  
  if (!name_validation$valid) {
    .icy_alert(name_validation$message)
    return(NULL)
  }
  
  var_name <- name_validation$name
  
  # Suggest package prefix if missing
  if (!startsWith(var_name, paste0(package_upper, "_"))) {
    suggested_name <- paste0(package_upper, "_", var_name)
    .icy_text(paste0("Suggested name: ", .apply_color(suggested_name, "yellow")))
    .icy_text(paste0("Use suggested name? ", .apply_color("(Y/n)", "gray")))
    response <- tolower(trimws(readline()))
    if (response != "n" && response != "no") {
      var_name <- suggested_name
    }
  }
  
  # Get default value
  .icy_text(paste0("Default value for ", .apply_color(var_name, "cyan"), 
                  " ", .apply_color("(or press Enter for NULL)", "gray"), ":"))
  default_value <- trimws(readline())
  
  if (nchar(default_value) == 0) {
    default_value <- NULL
  } else {
    # Try to parse value type
    default_value <- .parse_input_value(default_value)
  }
  
  # Get description (required)
  repeat {
    .icy_text(paste0("Description for ", .apply_color(var_name, "cyan"), " ", .apply_color("(required)", "gray"), ":"))
    description <- trimws(readline())
    
    if (nchar(description) > 0) {
      break
    }
    .icy_alert("Description is required")
  }
  
  # Detect and confirm type
  detected_type <- .detect_variable_type(default_value)
  .icy_text(paste0("Detected type: ", .apply_color(detected_type, "yellow")))
  .icy_text(paste0("Is this correct? ", .apply_color("(Y/n)", "gray")))
  response <- tolower(trimws(readline()))
  
  if (response == "n" || response == "no") {
    # Let user select type
    types <- c("character", "logical", "integer", "numeric", "path")
    .icy_text(.apply_color("Select type:", color = "brown"))
    .icy_bullets(types, bullet = "1:")
    .icy_text(paste0("Enter your choice: ", .apply_color("(1-5)", color = "gray")))
    
    repeat {
      selection <- trimws(readline())
      selection_num <- suppressWarnings(as.integer(selection))
      
      if (!is.na(selection_num) && selection_num >= 1 && selection_num <= length(types)) {
        type <- types[selection_num]
        break
      }
      .icy_alert("Please enter a valid number")
    }
  } else {
    type <- detected_type
  }
  
  # Optional note
  .icy_text(paste0("Note for ", .apply_color(var_name, "cyan"), " ", .apply_color("(optional, press Enter to skip)", "gray"), ":"))
  note <- trimws(readline())
  
  # Options for choice variables (skip for logical)
  options <- NULL
  if (type != "logical") {
    .icy_text(paste0("Valid options ", .apply_color("(comma-separated, or press Enter to skip)", "gray"), ":"))
    options_input <- trimws(readline())
    
    if (nchar(options_input) > 0) {
      options <- trimws(strsplit(options_input, ",")[[1]])
      options <- options[nchar(options) > 0]
    }
  }
  
  return(list(
    name = var_name,
    default = default_value,
    description = description,
    type = type,
    note = if(nchar(note) > 0) note else NULL,
    options = options
  ))
}

#' Add Variable to Template Data Structure
#'
#' Shared logic for adding a variable with metadata to the template data structure.
#' Handles NULL values properly and ensures consistent template structure.
#'
#' @param template_data Existing template data structure
#' @param var_info List with variable information from .collect_variable_interactively()
#' @return Modified template data structure
#' @keywords internal
.add_variable_to_template <- function(template_data, var_info) {
  # Initialize required sections if missing
  if (!"default" %in% names(template_data)) {
    template_data$default <- list()
  }
  
  # Add variable with proper NULL handling
  if (is.null(var_info$default)) {
    # Use the list merge technique for NULL
    temp_list <- list(NULL)
    names(temp_list) <- var_info$name
    template_data$default <- c(template_data$default, temp_list)
  } else {
    template_data$default[[var_info$name]] <- var_info$default
  }
  
  # Add metadata
  if (!is.null(var_info$description) && nchar(var_info$description) > 0) {
    if (!"descriptions" %in% names(template_data)) {
      template_data$descriptions <- list()
    }
    template_data$descriptions[[var_info$name]] <- var_info$description
  }
  
  if (!is.null(var_info$type)) {
    if (!"types" %in% names(template_data)) {
      template_data$types <- list()
    }
    template_data$types[[var_info$name]] <- var_info$type
  }
  
  if (!is.null(var_info$note) && nchar(var_info$note) > 0) {
    if (!"notes" %in% names(template_data)) {
      template_data$notes <- list()
    }
    template_data$notes[[var_info$name]] <- var_info$note
  }
  
  if (!is.null(var_info$options) && length(var_info$options) > 0) {
    if (!"options" %in% names(template_data)) {
      template_data$options <- list()
    }
    template_data$options[[var_info$name]] <- var_info$options
  }
  
  return(template_data)
}




#' Write Template with Header (Unified I/O)
#'
#' Unified function for writing template files with headers. This consolidates
#' the logic from .save_template_progress(), .write_template_safely(), and
#' write_config_yaml() template operations.
#'
#' @param template_data Template data structure to write
#' @param file_path Path to template file
#' @param package Package name for header generation
#' @param verbose Show messages
#' @param auto_save Whether this is an auto-save operation (affects messaging)
#' @return Logical indicating success (invisible)
#' @keywords internal
.write_template_with_header <- function(template_data, file_path, package, 
                                       verbose = TRUE, auto_save = FALSE) {
  
  # Create directory if it doesn't exist
  file_dir <- dirname(file_path)
  if (!dir.exists(file_dir)) {
    dir.create(file_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  tryCatch({
    # Generate or preserve header
    header <- if (file.exists(file_path) && auto_save) {
      # Preserve existing header only for auto-save operations
      .read_template_header(file_path)
    } else {
      # Generate new header for initial creation or overwrite
      .generate_template_header(package)
    }
    
    # Write using unified write_config_yaml approach
    .write_config_yaml(
      var_list = template_data,
      file_path = file_path,
      package = package,
      section = NULL,  # Template has its own structure
      custom_header = header,
      append_sections = FALSE,
      verbose = FALSE  # We handle messaging here
    )
    
    # Handle messaging
    if (verbose && !auto_save) {
      .icy_success(paste0("Template written: ", file_path))
    }
    
    return(invisible(TRUE))
    
  }, error = function(e) {
    .icy_stop(paste0("Error writing template file: ", e$message))
  })
}


#' Read Template Header Comments
#'
#' Extracts header comments from template files for preservation during writes.
#'
#' @param template_path Path to template file
#' @return Character vector of header lines
#' @keywords internal
.read_template_header <- function(template_path) {
  if (!file.exists(template_path)) {
    return(character(0))
  }
  
  lines <- readLines(template_path, warn = FALSE)
  header_lines <- character(0)
  
  # Get ALL section titles from centralized definitions to detect section comments
  section_titles <- character(0)
  all_definitions <- .get_metadata_definitions()
  if (!is.null(all_definitions)) {
    section_titles <- sapply(all_definitions, function(def) def$title, USE.NAMES = FALSE)
  }
  
  for (line in lines) {
    # Check if this line matches any section title pattern
    if (length(section_titles) > 0) {
      line_content <- gsub("^\\s*#\\s*", "", line)  # Remove leading # and whitespace
      is_section_comment <- any(sapply(section_titles, function(title) {
        # Check if line starts with this section title
        startsWith(line_content, title)
      }))
      
      if (is_section_comment) {
        # Stop reading header when we hit a section comment
        break
      }
    }
    
    if (grepl("^\\s*#", line)) {
      # Regular comment line - part of header
      header_lines <- c(header_lines, line)
    } else if (nchar(trimws(line)) == 0) {
      # Empty line - part of header
      header_lines <- c(header_lines, line)
    } else {
      # Stop at first non-comment, non-empty line
      break
    }
  }
  
  return(header_lines)
}


#' Streamlined Variable Collection for Create Mode
#'
#' Simplified interactive variable collection specifically for create_template().
#' Handles the streamlined "Add another variable? (Y/n)" flow with auto-save.
#'
#' @param template_data Initial template data structure
#' @param package Package name
#' @param template_path Path to template file for auto-save
#' @param verbose Show messages
#' @return Modified template data structure
#' @keywords internal
.streamlined_variable_collection <- function(template_data, package, template_path, 
                                           mode = "create", verbose = TRUE) {
  
  current_template_data <- template_data
  var_count <- 0
  
  if (verbose && mode == "create") {
    .icy_text("")
    .icy_title("Variable Configuration", level_adjust = -1)
    .icy_text("Let's add configuration variables to the template.")
  }
  
  repeat {
    var_count <- var_count + 1
    
    # For create mode, use streamlined prompting after first variable
    if (var_count > 1 && mode == "create") {
      .icy_text("")
      .icy_text(paste0("Add another variable? ", .apply_color("(Y/n)", "gray")))
      response <- tolower(trimws(readline()))
      if (response == "n" || response == "no") {
        break
      }
    }
    
    # Add section title for each variable in create mode
    if (verbose && mode == "create") {
      .icy_title(paste0("Variable ", var_count), level_adjust = 0)
    }
    
    tryCatch({
      # Get existing variables for validation
      metadata_sections <- .get_metadata_sections()
      existing_vars <- unique(unlist(lapply(
        current_template_data[!names(current_template_data) %in% metadata_sections],
        names
      )))
      
      # Get variable info interactively
      var_info <- .collect_variable_interactively(package, existing_vars)
      
      if (is.null(var_info)) {
        if (var_count == 1) {
          if (mode == "create") {
            .icy_alert("No variables added. Template creation cancelled.")
            return(NULL)  # Signal cancellation for create_template
          } else {
            return(current_template_data)  # Just return current data for other modes
          }
        } else {
          break  # User cancelled additional variable
        }
      }
      
      # Add variable using shared utilities
      current_template_data <- .add_variable_to_template(current_template_data, var_info)
      
      # Auto-save immediately after adding variable (for create mode)
      if (mode == "create" && !is.null(template_path)) {
        .write_template_with_header(
          template_data = current_template_data,
          file_path = template_path,
          package = package,
          verbose = FALSE,  # Don't show write messages during collection
          auto_save = TRUE
        )
      }
      
      if (verbose) {
        .icy_success(paste0("Added variable: ", var_info$name))
      }
      
    }, error = function(e) {
      .icy_alert(paste0("Error adding variable: ", e$message))
      if (var_count == 1 && mode == "create") {
        return(NULL)  # Signal cancellation
      }
    })
  }
  
  if (verbose && var_count > 1 && mode == "create") {
    .icy_success("Template completed successfully")
  }
  
  return(current_template_data)
}

#' Unified Template Variable Operation
#'
#' Consolidated function for adding or updating variables in template data.
#' This replaces the separate .add_template_variables() and .update_template_variables() functions.
#'
#' @param action Either "add" or "update"
#' @param template_data Template data structure
#' @param var_names Character vector of variable names
#' @param default_values Default values (list or single value)
#' @param descriptions Variable descriptions (optional)
#' @param types Variable types (optional) 
#' @param notes Variable notes (optional)
#' @param options Variable options (optional)
#' @param sections Target sections to modify
#' @param verbose Show messages
#' @return Modified template data structure
#' @keywords internal
.perform_template_operation <- function(action, template_data, var_names, default_values, 
                                       descriptions = NULL, types = NULL, notes = NULL, 
                                       options = NULL, sections = "default", verbose = TRUE) {
  
  # Ensure required sections exist for add operation
  if (action == "add" && !"default" %in% names(template_data)) {
    template_data$default <- list()
  }
  
  # Process each variable
  for (i in seq_along(var_names)) {
    var_name <- var_names[i]
    
    # Get values for this variable (handle both single values and lists)
    default_val <- if (is.list(default_values)) default_values[[i]] else default_values
    desc <- if (!is.null(descriptions)) {
      if (length(descriptions) > 1) descriptions[i] else descriptions
    } else NULL
    var_type <- if (!is.null(types)) {
      if (length(types) > 1) types[i] else types
    } else NULL
    var_note <- if (!is.null(notes)) {
      if (length(notes) > 1) notes[i] else notes
    } else NULL
    
    # Action-specific validation
    if (action == "add") {
      # Check if variable already exists
      if (var_name %in% names(template_data$default)) {
        .icy_warn(paste0("Variable ", var_name, " already exists. Use 'update' action instead."))
        next
      }
      
      # Auto-detect type if not provided
      if (is.null(var_type)) {
        var_type <- .detect_variable_type(default_val)
      }
      
    } else if (action == "update") {
      # Check if variable exists
      if (!"default" %in% names(template_data) || !var_name %in% names(template_data$default)) {
        .icy_warn(paste0("Variable ", var_name, " not found in template. Use 'add' action instead."))
        next
      }
    }
    
    # Update in specified sections
    if ("all" %in% sections) {
      # Return all data sections (exclude metadata)
      metadata_sections <- .get_metadata_sections()
      target_sections <- setdiff(names(template_data), metadata_sections)
    } else {
      target_sections <- sections
    }
    
    for (section in target_sections) {
      if (!section %in% names(template_data)) {
        template_data[[section]] <- list()
      }
      template_data[[section]][[var_name]] <- default_val
    }
    
    # Add/update metadata
    if (!is.null(desc) && nchar(desc) > 0) {
      if (!"descriptions" %in% names(template_data)) {
        template_data$descriptions <- list()
      }
      template_data$descriptions[[var_name]] <- desc
    }
    
    if (!is.null(var_type)) {
      if (!"types" %in% names(template_data)) {
        template_data$types <- list()
      }
      template_data$types[[var_name]] <- var_type
    }
    
    if (!is.null(var_note) && nchar(var_note) > 0) {
      if (!"notes" %in% names(template_data)) {
        template_data$notes <- list()
      }
      template_data$notes[[var_name]] <- var_note
    }
    
    if (!is.null(options) && length(options) > 0) {
      if (!"options" %in% names(template_data)) {
        template_data$options <- list()
      }
      template_data$options[[var_name]] <- options
    }
    
    if (verbose) {
      action_word <- if (action == "add") "Added" else "Updated"
      color <- if (action == "add") "green" else "blue"
      .icy_success(paste0(action_word, " ", .apply_color(var_name, color)))
    }
  }
  
  return(template_data)
}

