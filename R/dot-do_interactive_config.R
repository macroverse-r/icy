#' Perform Interactive Configuration
#'
#' Internal helper function to handle the interactive configuration process.
#' This is the core interactive dialog system that handles user input, option
#' selection, path validation, and directory creation dialogs.
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
#' @param allow_custom Whether to allow custom input when options exist
#' @param allow_create_dir Whether to allow directory creation for path types
#' @return Raw result value or NULL if skipped
#' @keywords internal
.do_interactive_config <- function(var_name,
                                   description,
                                   options, allow_skip,
                                   note, write, package,
                                   user, verbose,
                                   type, allow_custom, allow_create_dir) {
  
  # Display description if available
  if (!is.null(description) && nchar(description) > 0) {
    wrapped_description <- description
    .icy_title("Description", auto_number = FALSE, level_adjust = -1)
    .icy_text(wrapped_description)
  }
  
  # Display current value if available
  current_value <- tryCatch({
    get_value(var_name, package = package, user = user)
  }, error = function(e) NULL)
  
  if (!is.null(current_value)) {
    .icy_text("")
    current_display <- if (is.logical(current_value)) {
      toupper(as.character(current_value))  # TRUE/FALSE in caps
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
  .icy_title("Selection", auto_number = FALSE, level_adjust = -1)

  # Get user input based on whether options are available
  if (is.null(options) || length(options) == 0) {
    # Manual text input case
    is_manual_input <- TRUE
    
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
    success_msg <- .format_success_message(var_name, user_input, write)
    
  } else {
    # Options selection case
    is_manual_input <- FALSE
    
    if (allow_custom) {
      .icy_text("Select an option or enter custom value:")
    } else {
      .icy_text("Select an option:")
    }
    
    # Format options with current value indicator and resolved paths
    formatted_options <- character(length(options))
    for (i in seq_along(options)) {
      option_value <- options[i]
      
      # For path types, resolve special paths for display
      display_value <- if (!is.null(type) && type == "path") {
        resolved <- .resolve_special_path(option_value, package)
        
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
    .icy_text("")
    
    # Get user selection with retry loop
    repeat {
      if (allow_custom && allow_skip) {
        prompt_reminder <- paste0("(1-", length(options), ", 'c' for custom, or Enter to keep current config)")
      } else if (allow_custom && !allow_skip) {
        prompt_reminder <- paste0("(1-", length(options), " or 'c' for custom)")
      } else if (!allow_custom && allow_skip) {
        prompt_reminder <- paste0("(1-", length(options), " or Enter to keep current config)")
      } else {
        prompt_reminder <- paste0("(1-", length(options), ")")
      }
      prompt_text <- paste0("Enter your choice: ", .apply_color(prompt_reminder, color = "gray"))
      
      .icy_text(prompt_text, indentation = FALSE)
      user_input <- readline()
      
      # Handle skip case inside the loop
      if (is.null(.handle_skip_input(user_input, allow_skip))) return(NULL)
      
      # Handle custom input case
      if (allow_custom && tolower(trimws(user_input)) == "c") {
        # Switch to manual input mode
        if (type == "path") {
          # Path type with validation and retry loop
          repeat {
            .icy_text("Enter custom directory path:")
            custom_input <- readline()
            
            # Handle skip for custom input (though unlikely)
            if (is.null(.handle_skip_input(custom_input, allow_skip))) return(NULL)
            
            if (nchar(trimws(custom_input)) == 0) {
              .icy_alert("Path cannot be empty")
              next
            }
            
            # Resolve special paths and process path input
            resolved_custom_input <- .resolve_special_path(custom_input, package)
            path_result <- .process_path_input(resolved_custom_input, allow_create_dir = allow_create_dir)
            
            if (path_result$success) {
              selected_value <- path_result$path
              success_msg <- .format_success_message(var_name, selected_value, write)
              break
            } else {
              # Show error message only if there is one (for format errors)
              if (!is.null(path_result$message) && nchar(path_result$message) > 0) {
                .icy_alert(path_result$message)
              }
            }
          }
        } else {
          # Regular custom input for non-path types
          .icy_text("Enter custom value:")
          custom_input <- readline()
          
          # Handle skip for custom input
          if (is.null(.handle_skip_input(custom_input, allow_skip))) return(NULL)
          
          if (!allow_skip && nchar(trimws(custom_input)) == 0) {
            .icy_stop("A value is required")
          }
          
          selected_value <- custom_input
          success_msg <- .format_success_message(var_name, selected_value, write)
        }
        break
      }
      
      # Try to parse selection
      selection <- suppressWarnings(as.integer(user_input))
      
      if (!is.na(selection) && selection >= 1 && selection <= length(options)) {
        selected_value <- options[selection]
        
        # Check if selected value is the current value
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
          # Don't write - just return the current value
          .icy_success(success_msg)
          return(selected_value)
        }
        
        # For path types, resolve special paths and validate selected option
        if (!is.null(type) && type == "path") {
          # First resolve any special keywords or template variables
          resolved_path <- .resolve_special_path(selected_value, package)
          
          path_result <- .process_path_input(resolved_path, allow_create_dir = allow_create_dir)
          if (!path_result$success) {
            # Show error and continue the loop to let user try again
            if (!is.null(path_result$message) && nchar(path_result$message) > 0) {
              .icy_alert(path_result$message)
            }
            next  # Continue the option selection loop
          }
          selected_value <- path_result$path
        }
        
        success_msg <- .format_success_message(var_name, selected_value, write)
        break
      } else {
        if (allow_custom) {
          .icy_alert(paste0("Invalid selection. Please enter a number (1-", 
                           length(options), ") or 'c' for custom."))
        } else {
          .icy_alert("Invalid selection. Please try again.")
        }
      }
    }
  }
  
  # Apply path processing for manual input only (options already validated in loop)
  if (!is.null(type) && type == "path" && is_manual_input) {
    # First resolve any special keywords or template variables
    resolved_manual_input <- .resolve_special_path(selected_value, package)
    path_result <- .process_path_input(resolved_manual_input, allow_create_dir = allow_create_dir)
    if (!path_result$success) {
      .icy_stop(paste0("Manual path input is invalid: ", path_result$message))
    }
    selected_value <- path_result$path
    # Update success message to show cleaned path
    success_msg <- .format_success_message(var_name, selected_value, write)
  }
  
  # Common write and success handling for both paths
  success <- .write_config_value(var_name, selected_value, write, package, user, verbose, type)
  if (!success) {
    .icy_stop("Failed to write configuration")
  }
  
  .icy_success(success_msg)
  return(selected_value)
}