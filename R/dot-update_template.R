

#' Interactive Template Builder
#'
#' Main interactive interface for building and editing templates.
#'
#' @param template_data Existing template data structure
#' @param package Package name
#' @param template_file Path to template file
#' @param verbose Show messages
#' @param debug Show debug information
#' @return Modified template data structure
#' @keywords internal
.interactive_template_builder <- function(template_data, package, template_file, 
                                         verbose = TRUE, debug = FALSE) {
  
  # Check if template is empty (new template)
  all_vars <- unique(unlist(lapply(
    template_data[!names(template_data) %in% c("descriptions", "types", "notes", "options")],
    names
  )))
  is_empty_template <- length(all_vars) == 0
  
  if (verbose) {
    .icy_title("Interactive Template Editor")
    
    if (is_empty_template) {
      .icy_text("Template is empty")
      .icy_text("")
      .icy_title("Add New Variable", level_adjust = -1)
    } else {
      .show_template_summary(template_data, show_values = TRUE, show_metadata = TRUE)
    }
  }
  
  # For empty templates, start directly with adding a variable
  if (is_empty_template) {
    template_data <- .add_variable_interactive(template_data, package, verbose, debug)
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
  
  # Then proceed with the normal interactive loop
  repeat {
    .icy_text("")
    action <- .select_template_action()
    
    if (action == "done" || action == "quit") {
      break
    }
    
    template_data <- switch(action,
      "add" = .add_variable_interactive(template_data, package, verbose, debug),
      "update" = .update_variable_interactive(template_data, package, verbose),
      "remove" = .remove_variable_interactive(template_data, verbose),
      "section" = .manage_sections_interactive(template_data, package, verbose),
      "preview" = {
        .show_template_summary(template_data, show_values = TRUE, show_metadata = TRUE)
        template_data
      },
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
.select_template_action <- function() {
  .icy_title("Template Actions", level_adjust = -1)
  
  actions <- c(
    "Add new variable",
    "Update existing variable", 
    "Remove variable",
    "Manage sections",
    "Preview template",
    "Finish & Exit"
  )
  
  .icy_bullets(actions, bullet = "1.")
  
  .icy_text("")
  .icy_text("Select action (1-6):")
  
  repeat {
    choice <- trimws(readline())
    
    if (choice %in% as.character(1:6)) {
      return(switch(choice,
        "1" = "add",
        "2" = "update", 
        "3" = "remove",
        "4" = "section",
        "5" = "preview",
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
.add_variable_interactive <- function(template_data, package, verbose = TRUE, debug = FALSE) {
  .icy_title("Add New Variable", level_adjust = -1)
  
  # Get existing variables
  existing_vars <- unique(unlist(lapply(
    template_data[!names(template_data) %in% c("descriptions", "types", "notes", "options")],
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
.update_variable_interactive <- function(template_data, package, verbose = TRUE) {
  .icy_title("Update Variable", level_adjust = -1)
  
  # Get all variables
  all_vars <- unique(unlist(lapply(
    template_data[!names(template_data) %in% c("descriptions", "types", "notes", "options")],
    names
  )))
  
  if (length(all_vars) == 0) {
    .icy_alert("No variables to update")
    return(template_data)
  }
  
  # Select variable
  .icy_text("Select variable to update:")
  .icy_bullets(all_vars, bullet = "1.")
  
  .icy_text("")
  .icy_text(paste0("Enter number (1-", length(all_vars), "):"))
  
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
  .icy_text("Update fields (press Enter to keep current value):")
  
  # Update default value
  .icy_text(paste0("New default value (Enter for no change, 'NULL' for null):"))
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
  .icy_text("New description (Enter for no change):")
  new_desc <- trimws(readline())
  
  if (nchar(new_desc) > 0) {
    if (!"descriptions" %in% names(template_data)) {
      template_data$descriptions <- list()
    }
    template_data$descriptions[[var_name]] <- new_desc
  }
  
  # Update type
  .icy_text("New type (Enter for no change):")
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
.remove_variable_interactive <- function(template_data, verbose = TRUE) {
  .icy_title("Remove Variable", level_adjust = -1)
  
  # Get all variables
  all_vars <- unique(unlist(lapply(
    template_data[!names(template_data) %in% c("descriptions", "types", "notes", "options")],
    names
  )))
  
  if (length(all_vars) == 0) {
    .icy_alert("No variables to remove")
    return(template_data)
  }
  
  # Select variable
  .icy_text("Select variable to remove:")
  .icy_bullets(all_vars, bullet = "1.")
  
  .icy_text("")
  .icy_text(paste0("Enter number (1-", length(all_vars), "):"))
  
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
  .icy_text("Are you sure? (Y/n):")
  
  response <- tolower(trimws(readline()))
  if (response == "n" || response == "no") {
    .icy_alert("Removal cancelled")
    return(template_data)
  }
  
  # Remove from all sections
  removed_from <- character(0)
  
  # Remove from data sections
  for (section in names(template_data)) {
    if (!section %in% c("descriptions", "types", "notes", "options")) {
      if (var_name %in% names(template_data[[section]])) {
        template_data[[section]][[var_name]] <- NULL
        removed_from <- c(removed_from, section)
      }
    }
  }
  
  # Remove from metadata
  metadata_sections <- c("descriptions", "types", "notes", "options")
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
.manage_sections_interactive <- function(template_data, package, verbose = TRUE) {
  .icy_title("Manage Sections", level_adjust = -1)
  
  # Show current sections
  data_sections <- setdiff(names(template_data), 
                          c("descriptions", "types", "notes", "options"))
  
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
    "Back"
  )
  
  .icy_bullets(section_actions, bullet = "1.")
  
  .icy_text("")
  .icy_text("Select action (1-4):")
  
  choice <- trimws(readline())
  
  template_data <- switch(choice,
    "1" = .add_section_interactive(template_data, verbose),
    "2" = .copy_to_section_interactive(template_data, verbose),
    "3" = .remove_section_interactive(template_data, verbose),
    template_data
  )
  
  return(template_data)
}


#' Add Section Interactive
#' @keywords internal
.add_section_interactive <- function(template_data, verbose = TRUE) {
  .icy_text("")
  .icy_text("Enter new section name (e.g., production, development):")
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
    .icy_text("Copy variables from default section? (Y/n):")
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
  
  return(template_data)
}


#' Copy Variables to Section Interactive
#' @keywords internal
.copy_to_section_interactive <- function(template_data, verbose = TRUE) {
  # Get all available sections
  data_sections <- setdiff(names(template_data), 
                          c("descriptions", "types", "notes", "options"))
  
  if (length(data_sections) == 0) {
    .icy_alert("No sections available to copy from")
    return(template_data)
  }
  
  # Select source section
  .icy_text("")
  .icy_text("Select source section to copy from:")
  for (i in seq_along(data_sections)) {
    n_vars <- length(template_data[[data_sections[i]]])
    .icy_text(paste0("  ", i, ") ", data_sections[i], " (", n_vars, " variables)"))
  }
  
  .icy_text("")
  .icy_text(paste0("Enter number (1-", length(data_sections), "):"))
  
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
  .icy_text("Enter target section name (or press Enter to create new section):")
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
.remove_section_interactive <- function(template_data, verbose = TRUE) {
  data_sections <- setdiff(names(template_data), 
                          c("default", "descriptions", "types", "notes", "options"))
  
  if (length(data_sections) == 0) {
    .icy_alert("No sections to remove (cannot remove default or metadata)")
    return(template_data)
  }
  
  .icy_text("")
  .icy_text("Select section to remove:")
  
  # Create formatted section list with variable counts
  section_list <- sapply(data_sections, function(section) {
    n_vars <- length(template_data[[section]])
    paste0(section, " (", n_vars, " variables)")
  })
  
  .icy_bullets(section_list, bullet = "1.")
  
  .icy_text("")
  .icy_text(paste0("Enter number (1-", length(data_sections), "):"))
  
  repeat {
    choice <- trimws(readline())
    choice_num <- suppressWarnings(as.integer(choice))
    
    if (!is.na(choice_num) && choice_num >= 1 && choice_num <= length(data_sections)) {
      section_name <- data_sections[choice_num]
      break
    }
    
    .icy_alert("Please enter a valid number")
  }
  
  # Confirm
  .icy_text("")
  .icy_alert(paste0("Remove section '", section_name, "'?"))
  .icy_text("Are you sure? (Y/n):")
  
  response <- tolower(trimws(readline()))
  if (response == "n" || response == "no") {
    .icy_alert("Removal cancelled")
    return(template_data)
  }
  
  template_data[[section_name]] <- NULL
  
  if (verbose) {
    .icy_success(paste0("Removed section: ", section_name))
  }
  
  return(template_data)
}


