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
#' @param resolve_paths Path resolution mode ("ask", "static", "dynamic")
#' @param fn_tmpl Optional path to custom YAML template file (NULL uses default template)
#' @param fn_local Optional path to custom local YAML file (NULL uses default local file)
#' @return Raw result value or NULL if skipped
#' @keywords internal
.do_interactive_config <- function(var_name,
                                   description,
                                   options, allow_skip,
                                   note, write, package,
                                   user, verbose,
                                   type, allow_custom, allow_create_dir, resolve_paths, fn_tmpl = NULL, fn_local = NULL) {
  
  # Display description if available
  if (!is.null(description) && nchar(description) > 0) {
    wrapped_description <- description
    .icy_title("Description", auto_number = FALSE, level_adjust = -1)
    .icy_text(wrapped_description)
  }
  
  # Display current value if available
  current_value <- tryCatch({
    get_value(var_name, package = package, user = user, yaml_file = fn_local)
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
      if (is.null(.handle_skip_input(user_input, allow_skip))) return(NULL)
      
      # Validate input for manual entry
      if (!allow_skip && nchar(user_input) == 0) {
        .icy_stop("A value is required")
      }
      
      # Validate type if specified
      validation_error <- .validate_input_type(user_input, type)
      if (!is.null(validation_error)) {
        .icy_alert(validation_error)
        next  # Retry input
      }
      
      # Input is valid, break from loop
      break
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
    
    # Add helpful tip for path types with suffix support
    if (type == "path" && resolve_paths == "ask") {
      # Check if any options contain keywords
      keyword_indices <- which(sapply(options, .is_special_keyword))
      if (length(keyword_indices) > 0) {
        # Use the first keyword option number for examples
        example_num <- keyword_indices[1]
        example_keyword <- options[example_num]
        example_resolved <- .resolve_special_path(example_keyword, package)
        
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
    
    # Get user selection with retry loop
    repeat {
      # Clean prompt text without redundant suffix explanations
      if (type == "path" && resolve_paths == "ask") {
        if (allow_custom && allow_skip) {
          prompt_reminder <- paste0("(1-", length(options), ", 'c' for custom, or Enter to keep current config)")
        } else if (allow_custom && !allow_skip) {
          prompt_reminder <- paste0("(1-", length(options), " or 'c' for custom)")
        } else if (!allow_custom && allow_skip) {
          prompt_reminder <- paste0("(1-", length(options), " or Enter to keep current config)")
        } else {
          prompt_reminder <- paste0("(1-", length(options), ")")
        }
      } else {
        # Standard prompt text for non-path types or when resolution mode is fixed
        if (allow_custom && allow_skip) {
          prompt_reminder <- paste0("(1-", length(options), ", 'c' for custom, or Enter to keep current config)")
        } else if (allow_custom && !allow_skip) {
          prompt_reminder <- paste0("(1-", length(options), " or 'c' for custom)")
        } else if (!allow_custom && allow_skip) {
          prompt_reminder <- paste0("(1-", length(options), " or Enter to keep current config)")
        } else {
          prompt_reminder <- paste0("(1-", length(options), ")")
        }
      }
      prompt_text <- paste0("Enter your choice: ", .apply_color(prompt_reminder, color = "gray"))
      
      .icy_text(prompt_text)
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
          # Regular custom input for non-path types with validation loop
          repeat {
            .icy_text("Enter custom value:")
            custom_input <- readline()
            
            # Handle skip for custom input
            if (is.null(.handle_skip_input(custom_input, allow_skip))) return(NULL)
            
            if (!allow_skip && nchar(trimws(custom_input)) == 0) {
              .icy_stop("A value is required")
            }
            
            # Validate type if specified
            validation_error <- .validate_input_type(custom_input, type)
            if (!is.null(validation_error)) {
              .icy_alert(validation_error)
              next  # Retry input
            }
            
            # Input is valid, break from loop
            break
          }
          
          selected_value <- custom_input
          success_msg <- .format_success_message(var_name, selected_value, write)
        }
        break
      }
      
      # Parse selection with potential suffix
      parsed_input <- .parse_selection_input(user_input)
      selection <- parsed_input$selection
      selected_mode <- parsed_input$mode
      
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
        
        # For path types, handle resolution mode and validate
        if (!is.null(type) && type == "path") {
          # Determine final resolution mode
          final_resolution_mode <- if (selected_mode != "ask") {
            selected_mode  # User specified with suffix (s/d)
          } else if (resolve_paths != "ask") {
            resolve_paths  # Global setting (static/dynamic)
          } else if (.is_special_keyword(selected_value)) {
            # Ask user for resolution mode only for keyword options
            .icy_text("")
            resolved_path <- .resolve_special_path(selected_value, package)
            .icy_success(paste0("You selected: ", selected_value, " (", resolved_path, ")"))
            .icy_text("How should this be stored?")
            
            # Format resolution options and use icy_bullets with no additional bullet formatting
            resolution_options <- c(
              "s" = paste0("Store resolved path: ", resolved_path, " (static - always this exact location)"),
              "d" = paste0("Store keyword: '", selected_value, "' (dynamic - adjusts to current system)")
            )
            
            # Use icy_bullets with no bullet symbols, just the names
            .icy_bullets(resolution_options, bullet = "none")
            .icy_text("")
            
            repeat {
              .icy_text("Enter your choice: ('s'=static, 'd'=dynamic, or 'b' to go back)")
              resolution_choice <- trimws(tolower(readline()))
              
              if (resolution_choice == "s") {
                break_mode <- "static"
                break
              } else if (resolution_choice == "d") {
                break_mode <- "dynamic"
                break
              } else if (resolution_choice == "b") {
                # Go back to option selection
                .icy_text("")
                break_mode <- "back"
                break
              } else {
                .icy_alert("Invalid selection. Please enter 's', 'd', or 'b' to go back.")
              }
            }
            break_mode
          } else {
            "static"  # Non-keyword paths are always static
          }
          
          # Check if user chose to go back
          if (exists("break_mode") && break_mode == "back") {
            next  # Continue the outer option selection loop
          }
          
          # Process based on resolution mode
          if (final_resolution_mode == "dynamic" && .is_special_keyword(selected_value)) {
            # Store the keyword as-is for runtime resolution
            # Still validate it can be resolved currently
            test_resolved <- .resolve_special_path(selected_value, package)
            test_result <- .process_path_input(test_resolved, allow_create_dir = allow_create_dir)
            if (!test_result$success) {
              if (!is.null(test_result$message) && nchar(test_result$message) > 0) {
                .icy_alert(paste0("Keyword validation failed: ", test_result$message))
              }
              next
            }
            # Keep original keyword for dynamic resolution
            final_value <- selected_value
          } else {
            # Static resolution - resolve and validate now
            resolved_path <- .resolve_special_path(selected_value, package)
            path_result <- .process_path_input(resolved_path, allow_create_dir = allow_create_dir)
            if (!path_result$success) {
              if (!is.null(path_result$message) && nchar(path_result$message) > 0) {
                .icy_alert(path_result$message)
              }
              next
            }
            final_value <- path_result$path
          }
          
          selected_value <- final_value
        }
        
        # Enhanced success message for path resolution modes
        if (!is.null(type) && type == "path" && exists("final_resolution_mode")) {
          if (final_resolution_mode == "dynamic" && .is_special_keyword(selected_value)) {
            success_msg <- paste0("Set ", var_name, " to ", selected_value, " (dynamic) in ", 
                                 switch(write, "local" = "local config", "renviron" = ".Renviron", "session" = "session"))
          } else {
            success_msg <- paste0("Set ", var_name, " to ", selected_value, " (static) in ", 
                                 switch(write, "local" = "local config", "renviron" = ".Renviron", "session" = "session"))
          }
        } else {
          success_msg <- .format_success_message(var_name, selected_value, write)
        }
        break
      } else {
        # Enhanced error messages with suffix guidance
        if (type == "path" && resolve_paths == "ask") {
          if (allow_custom) {
            .icy_alert(paste0("Invalid selection. Please enter a number (1-", 
                             length(options), "), 'c' for custom, or add 's'/'d' suffix (e.g., '3s', '5d')."))
          } else {
            .icy_alert(paste0("Invalid selection. Please enter a number (1-", 
                             length(options), ") or add 's'/'d' suffix (e.g., '3s', '5d')."))
          }
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
  success <- .write_config_value(var_name, selected_value, write, package, user, verbose, type, fn_tmpl, fn_local)
  if (!success) {
    .icy_stop("Failed to write configuration")
  }
  
  .icy_success(success_msg)
  return(selected_value)
}
