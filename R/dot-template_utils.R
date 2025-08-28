
#' Generate Template Header
#'
#' Creates standard header comments for template files.
#'
#' @param package Package name
#' @param additional_lines Optional character vector of additional header lines
#' @return Character vector of header lines
#' @keywords internal
.generate_template_header <- function(package, additional_lines = NULL) {
  # Try to get header template from centralized YAML
  header_template <- .get_header_template()
  
  if (!is.null(header_template)) {
    # Use centralized template with substitutions
    header <- sapply(header_template, function(line) {
      line <- gsub("\\{PACKAGE\\}", toupper(package), line)
      line <- gsub("\\{DATE\\}", as.character(Sys.Date()), line)
      return(line)
    }, USE.NAMES = FALSE)
  } else {
    stop("Header template not found. Check that icy_metadata_sections.yml is available.")
  }
  
  if (!is.null(additional_lines)) {
    header <- c(header, "#", additional_lines)
  }
  
  # Return clean header without trailing blank line
  header
}



#' Show Next Steps
#'
#' Displays helpful next steps after template creation.
#'
#' @param package Package name
#' @param template_path Path to created template
#' @keywords internal
.show_next_steps <- function(package, template_path = NULL) {
  .icy_text("")
  .icy_title("Next Steps", level_adjust = -1)
  
  .icy_text("1. Add to your package's .onLoad() function:")
  .icy_text("   .onLoad <- function(libname, pkgname) {")
  .icy_text(paste0("     ", .apply_color("icy::create_local", "cyan"), "()"))
  .icy_text("   }")
  
  .icy_text("")
  .icy_text("2. Access configuration in your functions:")
  .icy_text(paste0("   config <- ", .apply_color("icy::get_config", "cyan"), "(origin = \"priority\")"))
  .icy_text(paste0("   api_key <- ", .apply_color(paste0("config$", toupper(package), "_API_KEY"), "yellow")))
  
  .icy_text("")
  .icy_text("3. Provide user configuration functions:")
  .icy_text(paste0("   ", .apply_color(paste0("configure_", package), "green"), " <- function() {"))
  .icy_text(paste0("     ", .apply_color("icy::setup", "cyan"), "(package = \"", package, "\")"))
  .icy_text("   }")
  
  if (!is.null(template_path)) {
    .icy_text("")
    .icy_text("4. Add variables to your template:")
    .icy_text(paste0("   ", .apply_color("icy::update_template", "cyan"), "(\"add\", \"NEW_VAR\", package = \"", package, "\")"))
  }
}


#' Display Template Summary
#'
#' Shows a summary of template contents.
#'
#' @param template_data Template data structure
#' @param show_values Logical; if TRUE, shows default values
#' @param show_metadata Logical; if TRUE, shows descriptions and types
#' @keywords internal
.show_template_summary <- function(template_data, 
                                  show_values = TRUE,
                                  show_metadata = TRUE) {
  
  .icy_title("Template Overview", level_adjust = -3)
  
  # Separate sections
  sections <- .separate_yaml_sections(template_data)
  
  # Count variables
  all_vars <- unique(unlist(lapply(sections$data, names)))
  n_vars <- length(all_vars)
  
  if (n_vars == 0) {
    .icy_text("Template is empty")
    return(invisible(NULL))
  }
  
  .icy_text(paste0("Template contains ", n_vars, " variable", 
                  if(n_vars != 1) "s" else ""))
  
  # Show data sections
  for (section_name in names(sections$data)) {
    section_vars <- names(sections$data[[section_name]])
    if (length(section_vars) > 0) {
      .icy_text("")
      .icy_text(paste0(.apply_color(section_name, "cyan"), " section:"))
      
      for (var_name in section_vars) {
        value <- sections$data[[section_name]][[var_name]]
        
        # Format value for display
        if (is.null(value)) {
          value_str <- .apply_color("NULL", "yellow")
        } else if (is.logical(value)) {
          value_str <- .apply_color(toupper(as.character(value)), "green")
        } else if (is.numeric(value)) {
          value_str <- .apply_color(as.character(value), "blue")
        } else {
          value_str <- paste0("'", value, "'")
        }
        
        # Build display line
        line <- paste0("  ", var_name)
        
        if (show_values) {
          line <- paste0(line, ": ", value_str)
        }
        
        if (show_metadata) {
          # Add type if available
          if ("types" %in% names(sections$metadata) &&
              var_name %in% names(sections$metadata$types)) {
            type_str <- sections$metadata$types[[var_name]]
            line <- paste0(line, " (", type_str, ")")
          }
          
          # Add description on next line if available
          if ("descriptions" %in% names(sections$metadata) &&
              var_name %in% names(sections$metadata$descriptions)) {
            .icy_text(line)
            desc_str <- sections$metadata$descriptions[[var_name]]
            .icy_text(paste0("    ", .apply_color(desc_str, "gray")))
          } else {
            .icy_text(line)
          }
        } else {
          .icy_text(line)
        }
      }
    }
  }
  
  # Summary of metadata
  if (show_metadata) {
    metadata_count <- sum(
      length(sections$metadata$descriptions),
      length(sections$metadata$types),
      length(sections$metadata$notes),
      length(sections$metadata$options)
    )
    
  }

  return(invisible(template_data))
}



