#' Interactive Template Builder
#'
#' Launches an interactive builder for modifying YAML configuration templates.
#' Provides a guided interface for adding, updating, and removing variables,
#' managing sections, and configuring inheritance.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()`
#'   to detect the calling package.
#' @param name Optional character string for named configs (e.g., "gams_switches").
#' @param backup Logical. If TRUE (default), creates a backup before modifications.
#' @param verbose Logical. If TRUE (default), displays progress messages.
#' @param template_data Optional pre-loaded template data. If provided, uses this
#'   instead of reading from file.
#'
#' @return Logical indicating success (invisible).
#'
#' @examples
#' \dontrun{
#' # Launch interactive template builder
#' update_template_interactive()
#'
#' # Build template for specific package
#' update_template_interactive(package = "mypackage")
#' }
#'
#' @export
update_template_interactive <- function(package = get_package_name(verbose = FALSE),
                                        name = NULL,
                                        backup = TRUE,
                                        verbose = TRUE,
                                        template_data = NULL) {

  # Find template file
  template_path <- .find_config_files(package = package, name = name)$fn_tmpl

  if (is.null(template_path)) {
    .icy_stop(paste0("No template configuration file found for package ", package))
  }

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
  updated_data <- ._update_template_interactive_builder(
    template_data = template_data,
    package = package,
    template_file = template_path,
    verbose = verbose,
    debug = FALSE
  )

  # Write final template
  .write_template_with_header(
    template_data = updated_data,
    file_path = template_path,
    package = package,
    verbose = verbose,
    auto_save = FALSE
  )

  return(invisible(TRUE))
}


# Internal functions used only by update_template_interactive() ----

#' Interactive Template Builder
#' @keywords internal
._update_template_interactive_builder <- function(template_data, package, template_file, 
                                         verbose = TRUE, debug = FALSE) {
  
  # Check if template is empty (new template)
  metadata_sections <- .get_metadata_sections()
  all_vars <- unique(unlist(lapply(
    template_data[!names(template_data) %in% metadata_sections],
    names
  )))
  is_empty_template <- length(all_vars) == 0
  
  # For empty templates, start directly with adding a variable
  if (is_empty_template) {
    if (verbose) {
      empty_msg <- "Template is empty... (automatically opting for adding a first variable)"
      .icy_text(.apply_color(empty_msg, "gray"))
    }
    template_data <- ._update_template_add_variable_interactive(template_data, package, verbose, debug)
    # Auto-save after adding the first variable
    if (!is.null(template_file) && file.exists(template_file)) {
      .write_template_with_header(
        template_data = template_data,
        file_path = template_file,
        package = package,
        verbose = verbose,
        auto_save = TRUE
      )
    }
  }
  
  repeat {
    .icy_text("")
    action <- ._update_template_select_action()
    
    if (action == "done" || action == "quit") {
      break
    }
    
    template_data <- switch(action,
      "add" = ._update_template_add_variable_interactive(template_data, package, verbose, debug),
      "update" = ._update_template_update_variable_interactive(template_data, package, verbose),
      "remove" = ._update_template_remove_variable_interactive(template_data, verbose),
      "section" = ._update_template_manage_sections_interactive(template_data, package, verbose),
      "view" = .show_template_summary(template_data, show_values = TRUE, show_metadata = TRUE),
      template_data
    )
    
    # Auto-save after each change
    if (action %in% c("add", "update", "remove", "section")) {
      if (!is.null(template_file) && file.exists(template_file)) {
        .write_template_with_header(
          template_data = template_data,
          file_path = template_file,
          package = package,
          verbose = verbose,
          auto_save = TRUE
        )
      }
    }
  }
  
  return(template_data)
}

#' Select Template Action
#' @keywords internal
._update_template_select_action <- function() {
  actions <- c(
    "Add new variable",
    "Update existing variable", 
    "Remove variable",
    "Manage sections",
    "View template",
    "Finish & Exit"
  )
  
  .icy_text(.apply_color("-----------", color = "gray"))
  .icy_text(.apply_color("Select an option:", color = "brown"))
  .icy_bullets(actions, bullet = "1:")
  .icy_text(paste0("Enter your choice: ", .apply_color("(1-6)", color = "gray")))
  
  repeat {
    choice <- trimws(readline())
    
    if (choice %in% as.character(1:6)) {
      .icy_text(.apply_color("-----------", color = "gray"))
      return(switch(choice,
                    "1" = "add",
                    "2" = "update", 
                    "3" = "remove",
                    "4" = "section",
                    "5" = "view",
                    "6" = "done"
                    ))
    } else if (tolower(choice) %in% c("q", "quit", "exit")) {
      return("quit")
    }
    
    .icy_alert("Please enter a number 1-6, or 'q' to quit")
  }
}

#' Add Variable Interactive
#' @keywords internal
._update_template_add_variable_interactive <- function(template_data, package, verbose = TRUE, debug = FALSE) {
  .icy_title("Adding New Variable", level_adjust = -3)
  
  # Get existing variables
  metadata_sections <- .get_metadata_sections()
  existing_vars <- unique(unlist(lapply(
    template_data[!names(template_data) %in% metadata_sections],
    names
  )))
  
  # Collect variable information using shared utility
  var_info <- .collect_variable_interactively(package, existing_vars)
  
  if (is.null(var_info)) {
    .icy_alert("Variable addition cancelled")
    return(template_data)
  }
  
  # Use shared utility to add variable to template
  template_data <- .add_variable_to_template(template_data, var_info)
  
  if (verbose) {
    .icy_success(paste0("Added variable: ", var_info$name))
  }
  
  return(template_data)
}

#' Update Variable Interactive
#' @keywords internal
._update_template_update_variable_interactive <- function(template_data, package, verbose = TRUE) {
  .icy_title("Updating Variable", level_adjust = -3)
  
  # Get all variables
  metadata_sections <- .get_metadata_sections()
  all_vars <- unique(unlist(lapply(
    template_data[!names(template_data) %in% metadata_sections],
    names
  )))
  
  if (length(all_vars) == 0) {
    .icy_alert("No variables to update")
    return(template_data)
  }
  
  # Select variable
  .icy_text(.apply_color("Select variable to update:", color = "brown"))
  .icy_bullets(all_vars, bullet = "1:")
  .icy_text(paste0("Enter your choice: ", .apply_color(paste0("(1-", length(all_vars), ")"), color = "gray")))
  
  repeat {
    choice <- trimws(readline())
    choice_num <- suppressWarnings(as.integer(choice))
    
    if (!is.na(choice_num) && choice_num >= 1 && choice_num <= length(all_vars)) {
      var_name <- all_vars[choice_num]
      break
    }
    
    .icy_alert("Please enter a valid number")
  }
  
  # Show current values
  .icy_text("")
  .icy_text(paste0("Current values for ", .apply_color(var_name, "cyan"), ":"))
  
  # Get current values from default section
  current_default <- if ("default" %in% names(template_data) && 
                        var_name %in% names(template_data$default)) {
    template_data$default[[var_name]]
  } else {
    NULL
  }
  
  .icy_text(paste0("  Default: ", 
    if (is.null(current_default)) "NULL" else as.character(current_default)))
  
  if ("descriptions" %in% names(template_data) && 
      var_name %in% names(template_data$descriptions)) {
    .icy_text(paste0("  Description: ", template_data$descriptions[[var_name]]))
  }
  
  if ("types" %in% names(template_data) && 
      var_name %in% names(template_data$types)) {
    .icy_text(paste0("  Type: ", template_data$types[[var_name]]))
  }
  
  # Update each field
  .icy_text("")
  .icy_text(paste0("Update fields ", .apply_color("(press Enter to keep current value)", "gray"), ":"))
  
  # Update default value
  .icy_text(paste0("New default value ", .apply_color("(Enter for no change, 'NULL' for null)", "gray"), ":"))
  new_default <- trimws(readline())
  
  if (nchar(new_default) > 0) {
    if (toupper(new_default) == "NULL") {
      # Use NULL
      temp_list <- list(NULL)
      names(temp_list) <- var_name
      if ("default" %in% names(template_data)) {
        # Remove old value first
        template_data$default[[var_name]] <- NULL
        # Add with NULL preservation
        template_data$default <- c(template_data$default, temp_list)
      }
    } else {
      # Parse and set new value
      template_data$default[[var_name]] <- .parse_input_value(new_default)
    }
  }
  
  # Update description
  .icy_text(paste0("New description ", .apply_color("(Enter for no change)", "gray"), ":"))
  new_desc <- trimws(readline())
  
  if (nchar(new_desc) > 0) {
    if (!"descriptions" %in% names(template_data)) {
      template_data$descriptions <- list()
    }
    template_data$descriptions[[var_name]] <- new_desc
  }
  
  # Update type
  .icy_text(paste0("New type ", .apply_color("(Enter for no change)", "gray"), ":"))
  new_type <- trimws(readline())
  
  if (nchar(new_type) > 0) {
    if (!"types" %in% names(template_data)) {
      template_data$types <- list()
    }
    template_data$types[[var_name]] <- new_type
  }
  
  if (verbose) {
    .icy_success(paste0("Updated variable: ", var_name))
  }
  
  return(template_data)
}

#' Remove Variable Interactive
#' @keywords internal
._update_template_remove_variable_interactive <- function(template_data, verbose = TRUE) {
  .icy_title("Removing Variable", level_adjust = -3)
  
  # Get all variables
  metadata_sections <- .get_metadata_sections()
  all_vars <- unique(unlist(lapply(
    template_data[!names(template_data) %in% metadata_sections],
    names
  )))
  
  if (length(all_vars) == 0) {
    .icy_alert("No variables to remove")
    return(template_data)
  }
  
  # Select variable
  .icy_text(.apply_color("Select variable to remove:", color = "brown"))
  .icy_bullets(all_vars, bullet = "1:")
  .icy_text(paste0("Enter your choice: ", .apply_color(paste0("(1-", length(all_vars), ")"), color = "gray")))
  
  repeat {
    choice <- trimws(readline())
    choice_num <- suppressWarnings(as.integer(choice))
    
    if (!is.na(choice_num) && choice_num >= 1 && choice_num <= length(all_vars)) {
      var_name <- all_vars[choice_num]
      break
    }
    
    .icy_alert("Please enter a valid number")
  }
  
  # Confirm removal
  .icy_text("")
  .icy_alert(paste0("This will remove '", var_name, "' from all sections"))
  .icy_text(paste0("Are you sure? ", .apply_color("(Y/n)", "gray")))
  
  response <- tolower(trimws(readline()))
  if (response == "n" || response == "no") {
    .icy_alert("Removal cancelled")
    return(template_data)
  }
  
  # Remove from all sections
  removed_from <- character(0)
  
  # Remove from data sections
  metadata_sections <- .get_metadata_sections()
  for (section in names(template_data)) {
    if (!section %in% metadata_sections) {
      if (var_name %in% names(template_data[[section]])) {
        template_data[[section]][[var_name]] <- NULL
        removed_from <- c(removed_from, section)
      }
    }
  }
  
  # Remove from metadata
  for (section in metadata_sections) {
    if (section %in% names(template_data) && 
        var_name %in% names(template_data[[section]])) {
      template_data[[section]][[var_name]] <- NULL
    }
  }
  
  # Clean empty sections
  template_data <- .clean_yaml_structure(template_data, remove_empty = TRUE)
  
  if (verbose) {
    if (length(removed_from) > 0) {
      .icy_success(paste0("Removed '", var_name, "' from: ", 
                         paste(removed_from, collapse = ", ")))
    } else {
      .icy_alert(paste0("Variable '", var_name, "' not found"))
    }
  }
  
  return(template_data)
}

#' Manage Sections Interactive
#' @keywords internal
._update_template_manage_sections_interactive <- function(template_data, package, verbose = TRUE) {
  .icy_title("Managing Sections", level_adjust = -3)
  
  # Show current sections
  metadata_sections <- .get_metadata_sections()
  data_sections <- setdiff(names(template_data), metadata_sections)
  
  .icy_text("Current sections:")
  if (length(data_sections) > 0) {
    for (section in data_sections) {
      n_vars <- length(template_data[[section]])
      .icy_text(paste0("  - ", section, " (", n_vars, " variables)"))
    }
  } else {
    .icy_text("  (none)")
  }
  
  .icy_text("")
  
  section_actions <- c(
    "Add new section",
    "Copy variables to section", 
    "Remove section",
    "Configure section inheritance",
    "Back"
  )
  
  .icy_text(.apply_color("Select section action:", color = "brown"))
  .icy_bullets(section_actions, bullet = "1:")
  .icy_text(paste0("Enter your choice: ", .apply_color("(1-5)", color = "gray")))
  
  choice <- trimws(readline())
  
  template_data <- switch(choice,
    "1" = ._update_template_add_section_interactive(template_data, verbose),
    "2" = ._update_template_copy_to_section_interactive(template_data, verbose),
    "3" = ._update_template_remove_section_interactive(template_data, verbose),
    "4" = ._update_template_configure_inheritance_interactive(template_data, verbose),
    template_data
  )
  
  return(template_data)
}

#' Add Section Interactive
#' @keywords internal
._update_template_add_section_interactive <- function(template_data, verbose = TRUE) {
  .icy_text("")
  .icy_text(paste0("Enter new section name ", .apply_color("(e.g., production, development)", "gray"), ":"))
  section_name <- trimws(readline())
  
  if (nchar(section_name) == 0) {
    .icy_alert("Section name cannot be empty")
    return(template_data)
  }
  
  if (section_name %in% names(template_data)) {
    .icy_alert(paste0("Section '", section_name, "' already exists"))
    return(template_data)
  }
  
  # Create empty section
  template_data[[section_name]] <- list()
  
  # Ask if user wants to copy variables from default
  if ("default" %in% names(template_data) && length(template_data$default) > 0) {
    .icy_text("")
    .icy_text(paste0("Copy variables from default section? ", .apply_color("(Y/n)", "gray")))
    response <- tolower(trimws(readline()))
    
    if (response != "n" && response != "no") {
      template_data[[section_name]] <- template_data$default
      .icy_success(paste0("Created section '", section_name, 
                         "' with ", length(template_data$default), " variables"))
    } else {
      .icy_success(paste0("Created empty section '", section_name, "'"))
    }
  } else {
    .icy_success(paste0("Created empty section '", section_name, "'"))
  }
  
  # Ask about inheritance
  metadata_sections <- .get_metadata_sections()
  other_sections <- setdiff(names(template_data), c(section_name, metadata_sections))
  
  if (length(other_sections) > 0) {
    .icy_text("")
    .icy_text(paste0("Should '", section_name, "' inherit from another section? ", 
                    .apply_color("(Y/n)", "gray")))
    response <- tolower(trimws(readline()))
    
    if (response != "n" && response != "no") {
      .icy_text(.apply_color("Select parent section:", color = "brown"))
      .icy_bullets(other_sections, bullet = "1:")
      .icy_text(paste0("Enter your choice: ", 
                      .apply_color(paste0("(1-", length(other_sections), ")"), color = "gray")))
      
      repeat {
        choice <- trimws(readline())
        choice_num <- suppressWarnings(as.integer(choice))
        
        if (!is.na(choice_num) && choice_num >= 1 && choice_num <= length(other_sections)) {
          parent_section <- other_sections[choice_num]
          
          # Temporarily add the inheritance to validate
          temp_data <- template_data
          if (!"inheritances" %in% names(temp_data)) {
            temp_data$inheritances <- list()
          }
          temp_data$inheritances[[section_name]] <- parent_section
          
          # Validate the temporary structure
          validation_result <- .validate_template_inheritance(temp_data)
          if (!validation_result$valid) {
            .icy_alert("Cannot set inheritance: would create circular dependency")
          } else {
            # If valid, apply the change to the real data
            if (!"inheritances" %in% names(template_data)) {
              template_data$inheritances <- list()
            }
            template_data$inheritances[[section_name]] <- parent_section
            .icy_text(paste0("  -> Set inheritance: '", section_name, "' inherits from '", parent_section, "'"))
          }
          break
        }
        
        .icy_alert("Please enter a valid number")
      }
    }
  }
  
  return(template_data)
}

#' Copy to Section Interactive
#' @keywords internal
._update_template_copy_to_section_interactive <- function(template_data, verbose = TRUE) {
  # Get all available sections
  metadata_sections <- .get_metadata_sections()
  data_sections <- setdiff(names(template_data), metadata_sections)
  
  if (length(data_sections) == 0) {
    .icy_alert("No sections available to copy from")
    return(template_data)
  }
  
  # Select source section
  .icy_text("")
  # Create formatted section list with variable counts
  section_list <- sapply(data_sections, function(section) {
    n_vars <- length(template_data[[section]])
    paste0(section, " (", n_vars, " variables)")
  })
  .icy_text(.apply_color("Select source section to copy from:", color = "brown"))
  .icy_bullets(section_list, bullet = "1:")
  .icy_text(paste0("Enter your choice: ", .apply_color(paste0("(1-", length(data_sections), ")"), color = "gray")))
  
  repeat {
    choice <- trimws(readline())
    choice_num <- suppressWarnings(as.integer(choice))
    
    if (!is.na(choice_num) && choice_num >= 1 && choice_num <= length(data_sections)) {
      source_section <- data_sections[choice_num]
      break
    }
    .icy_alert("Please enter a valid number")
  }
  
  # Get variables from source section
  source_vars <- names(template_data[[source_section]])
  
  if (length(source_vars) == 0) {
    .icy_alert(paste0("No variables in section '", source_section, "'"))
    return(template_data)
  }
  
  # Select variables to copy (for now, copy all - could be enhanced later)
  .icy_text("")
  .icy_text(paste0("Variables in '", source_section, "': ", paste(source_vars, collapse = ", ")))
  
  # Select or create target section
  .icy_text("")
  .icy_text(paste0("Enter target section name ", .apply_color("(or press Enter to create new section)", "gray"), ":"))
  target_section <- trimws(readline())
  
  if (nchar(target_section) == 0) {
    .icy_text("Enter name for new section:")
    target_section <- trimws(readline())
    
    if (nchar(target_section) == 0) {
      .icy_alert("Section name cannot be empty")
      return(template_data)
    }
  }
  
  # Create target section if it doesn't exist
  if (!target_section %in% names(template_data)) {
    template_data[[target_section]] <- list()
  }
  
  # Copy variables
  for (var_name in source_vars) {
    template_data[[target_section]][[var_name]] <- template_data[[source_section]][[var_name]]
  }
  
  if (verbose) {
    .icy_success(paste0("Copied ", length(source_vars), " variable", 
                       if(length(source_vars) != 1) "s" else "", 
                       " from '", source_section, "' to '", target_section, "'"))
  }
  
  return(template_data)
}

#' Remove Section Interactive
#' @keywords internal
._update_template_remove_section_interactive <- function(template_data, verbose = TRUE) {
  data_sections <- setdiff(names(template_data),
                          c("default", .get_metadata_sections()))
  
  if (length(data_sections) == 0) {
    .icy_alert("No sections to remove (cannot remove default or metadata)")
    return(template_data)
  }
  
  .icy_text("")
  .icy_text(.apply_color("Select section to remove:", color = "brown"))
  .icy_bullets(data_sections, bullet = "1:")
  .icy_text(paste0("Enter your choice: ", .apply_color(paste0("(1-", length(data_sections), ")"), color = "gray")))
  
  repeat {
    choice <- trimws(readline())
    choice_num <- suppressWarnings(as.integer(choice))
    
    if (!is.na(choice_num) && choice_num >= 1 && choice_num <= length(data_sections)) {
      section_name <- data_sections[choice_num]
      break
    }
    .icy_alert("Please enter a valid number")
  }
  
  # Confirm removal
  .icy_text("")
  .icy_alert(paste0("This will remove section '", section_name, "' and all its variables"))
  .icy_text(paste0("Are you sure? ", .apply_color("(Y/n)", "gray")))
  
  response <- tolower(trimws(readline()))
  if (response == "n" || response == "no") {
    .icy_alert("Section removal cancelled")
    return(template_data)
  }
  
  # Remove section
  template_data[[section_name]] <- NULL
  
  # Remove any inheritance references
  if ("inheritances" %in% names(template_data)) {
    # Remove this section as a child
    if (section_name %in% names(template_data$inheritances)) {
      template_data$inheritances[[section_name]] <- NULL
    }
    
    # Remove this section as a parent
    for (child in names(template_data$inheritances)) {
      if (template_data$inheritances[[child]] == section_name) {
        template_data$inheritances[[child]] <- NULL
      }
    }
    
    # Clean empty inheritances section
    if (length(template_data$inheritances) == 0) {
      template_data$inheritances <- NULL
    }
  }
  
  if (verbose) {
    .icy_success(paste0("Removed section '", section_name, "'"))
  }
  
  return(template_data)
}

#' Configure Inheritance Interactive
#' @keywords internal
._update_template_configure_inheritance_interactive <- function(template_data, verbose = TRUE) {
  # Get data sections (exclude metadata)
  metadata_sections <- .get_metadata_sections()
  data_sections <- setdiff(names(template_data), metadata_sections)
  
  if (length(data_sections) <= 1) {
    .icy_alert("Need at least 2 sections to configure inheritance")
    return(template_data)
  }
  
  inheritance_actions <- c(
    "Set inheritance relationship",
    "Remove inheritance relationship", 
    "Clear all inheritance",
    "View inheritance chain",
    "Back"
  )
  
  .icy_text("")
  .icy_text(.apply_color("Select inheritance action:", color = "brown"))
  .icy_bullets(inheritance_actions, bullet = "1:")
  .icy_text(paste0("Enter your choice: ", .apply_color("(1-5)", color = "gray")))
  
  choice <- trimws(readline())
  
  template_data <- switch(choice,
    "1" = ._update_template_set_inheritance_interactive(template_data, data_sections, verbose),
    "2" = ._update_template_remove_inheritance_interactive(template_data, data_sections, verbose),
    "3" = ._update_template_clear_inheritance_interactive(template_data, verbose),
    "4" = ._update_template_view_inheritance_chain_interactive(template_data, data_sections, verbose),
    template_data
  )
  
  return(template_data)
}

#' Set Inheritance Interactive
#' @keywords internal
._update_template_set_inheritance_interactive <- function(template_data, data_sections, verbose = TRUE) {
  .icy_text("")
  .icy_text(.apply_color("Select section to configure:", color = "brown"))
  .icy_bullets(data_sections, bullet = "1:")
  .icy_text(paste0("Enter your choice: ", .apply_color(paste0("(1-", length(data_sections), ")"), color = "gray")))
  
  repeat {
    choice <- trimws(readline())
    choice_num <- suppressWarnings(as.integer(choice))
    
    if (!is.na(choice_num) && choice_num >= 1 && choice_num <= length(data_sections)) {
      child_section <- data_sections[choice_num]
      break
    }
    .icy_alert("Please enter a valid number")
  }
  
  # Get possible parent sections (exclude the child itself)
  parent_options <- setdiff(data_sections, child_section)
  
  .icy_text("")
  .icy_text(.apply_color("Select parent section:", color = "brown"))
  .icy_bullets(parent_options, bullet = "1:")
  .icy_text(paste0("Enter your choice: ", .apply_color(paste0("(1-", length(parent_options), ")"), color = "gray")))
  
  repeat {
    choice <- trimws(readline())
    choice_num <- suppressWarnings(as.integer(choice))
    
    if (!is.na(choice_num) && choice_num >= 1 && choice_num <= length(parent_options)) {
      parent_section <- parent_options[choice_num]
      break
    }
    .icy_alert("Please enter a valid number")
  }
  
  # Temporarily add the inheritance to validate
  temp_data <- template_data
  if (!"inheritances" %in% names(temp_data)) {
    temp_data$inheritances <- list()
  }
  temp_data$inheritances[[child_section]] <- parent_section
  
  # Validate the temporary structure
  validation_result <- .validate_template_inheritance(temp_data)
  if (!validation_result$valid) {
    .icy_alert("Cannot set inheritance: would create circular dependency")
    return(template_data)
  }
  
  # If valid, apply the change to the real data
  if (!"inheritances" %in% names(template_data)) {
    template_data$inheritances <- list()
  }
  template_data$inheritances[[child_section]] <- parent_section
  
  if (verbose) {
    .icy_success(paste0("Set inheritance: '", child_section, "' inherits from '", parent_section, "'"))
  }
  
  return(template_data)
}

#' Remove Inheritance Interactive
#' @keywords internal
._update_template_remove_inheritance_interactive <- function(template_data, data_sections, verbose = TRUE) {
  if (!"inheritances" %in% names(template_data) || length(template_data$inheritances) == 0) {
    .icy_alert("No inheritance relationships to remove")
    return(template_data)
  }
  
  inherit_relationships <- names(template_data$inheritances)
  relationship_display <- sapply(inherit_relationships, function(child) {
    parent <- template_data$inheritances[[child]]
    paste0(child, " inherits from ", parent)
  })
  
  .icy_text("")
  .icy_text(.apply_color("Select inheritance to remove:", color = "brown"))
  .icy_bullets(relationship_display, bullet = "1:")
  .icy_text(paste0("Enter your choice: ", .apply_color(paste0("(1-", length(inherit_relationships), ")"), color = "gray")))
  
  repeat {
    choice <- trimws(readline())
    choice_num <- suppressWarnings(as.integer(choice))
    
    if (!is.na(choice_num) && choice_num >= 1 && choice_num <= length(inherit_relationships)) {
      child_to_remove <- inherit_relationships[choice_num]
      break
    }
    .icy_alert("Please enter a valid number")
  }
  
  # Remove inheritance
  parent_removed <- template_data$inheritances[[child_to_remove]]
  template_data$inheritances[[child_to_remove]] <- NULL
  
  # Clean empty inheritances section
  if (length(template_data$inheritances) == 0) {
    template_data$inheritances <- NULL
  }
  
  if (verbose) {
    .icy_success(paste0("Removed inheritance: '", child_to_remove, "' no longer inherits from '", parent_removed, "'"))
  }
  
  return(template_data)
}

#' Clear Inheritance Interactive
#' @keywords internal
._update_template_clear_inheritance_interactive <- function(template_data, verbose = TRUE) {
  if (!"inheritances" %in% names(template_data) || length(template_data$inheritances) == 0) {
    .icy_alert("No inheritance relationships to clear")
    return(template_data)
  }
  
  n_relationships <- length(template_data$inheritances)
  .icy_text("")
  .icy_alert(paste0("This will remove all ", n_relationships, " inheritance relationship", 
                   if(n_relationships != 1) "s" else ""))
  .icy_text(paste0("Are you sure? ", .apply_color("(Y/n)", "gray")))
  
  response <- tolower(trimws(readline()))
  if (response == "n" || response == "no") {
    .icy_alert("Clear inheritance cancelled")
    return(template_data)
  }
  
  template_data$inheritances <- NULL
  
  if (verbose) {
    .icy_success(paste0("Cleared all inheritance relationships"))
  }
  
  return(template_data)
}

#' View Inheritance Chain Interactive
#' @keywords internal
._update_template_view_inheritance_chain_interactive <- function(template_data, data_sections, verbose = TRUE) {
  if (!"inheritances" %in% names(template_data) || length(template_data$inheritances) == 0) {
    .icy_alert("No inheritance relationships defined")
    return(template_data)
  }
  
  .icy_text("")
  .icy_text("Current inheritance relationships:")
  
  # Show all relationships
  for (child in names(template_data$inheritances)) {
    parent <- template_data$inheritances[[child]]
    .icy_text(paste0("  ", child, " -> ", parent))
  }
  
  .icy_text("")
  .icy_text("Press Enter to continue...")
  readline()
  
  return(template_data)
}



