#' Get Environment Variable Configuration
#'
#' Reads environment variable configuration from different origins (template, local, .Renviron, 
#' or merged with priority resolution). This function is typically used within an R package to 
#' retrieve environment variable configurations based on the specified origin.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param origin Character string specifying where to read the configuration from:
#'   - "template": Read from the package's template YAML file (read-only blueprint)
#'   - "local": Read from the user's local configuration file (default)
#'   - "renviron": Read from .Renviron file (filtered by package prefix)
#'   - "priority": Merge all sources with priority order: Session > .Renviron > Local > Template.
#'     Template values serve as defaults that are overridden by higher priority sources.
#' @param section Character string for the section in the YAML file (default: "default").
#' @param fn_tmpl Character string with the name or path to the template YAML file. 
#'   If NULL, uses default template for the package.
#' @param fn_local Character string with the name or path to the local YAML file.
#'   If NULL, uses default local config for the package.
#' @param case_format Character string indicating the case format to use for
#'   searching YAML files if no specific files are provided. Options are:
#'   "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param inherit Character string specifying a section to inherit values from, or
#'   0 to explicitly disable inheritance. If NULL (default), the function checks 
#'   for an "inheritances" section in the template that defines automatic inheritance 
#'   relationships. When explicitly specified, this parameter overrides any 
#'   template-defined inheritance. Use 0 to disable inheritance even if defined 
#'   in template. Values from the inherit section are used as defaults, which can 
#'   be overridden by the main section. Inheritance is recursive - if A inherits 
#'   from B and B inherits from C, A will receive values from both B and C. 
#'   Only works with "template" and "local" origins.
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
#' @param validate Logical. If TRUE (default), validates the template structure
#'   and checks for issues like circular inheritance. Set to FALSE to skip 
#'   validation for performance or to work with templates that have known issues.
#' @param confirm_fuzzy Logical. If TRUE (default), prompts for confirmation when fuzzy 
#'   matches are used in file searching. Set to FALSE for non-interactive use.
#'
#' @return Named list of environment variable configurations.
#'
#' @examples
#' \dontrun{
#' # Get configuration from template (package defaults)
#' template_config <- get_config(package = "mypackage", origin = "template")
#'
#' # Get configuration from local file (user customizations)
#' local_config <- get_config(package = "mypackage", origin = "local")
#'
#' # Get merged configuration with priority resolution
#' # Template values are used as base, overridden by local/.Renviron/session
#' # If no local config exists, template values are still returned
#' config <- get_config(package = "mypackage", origin = "priority")
#'
#' # Get production config with defaults inherited from default section
#' prod_config <- get_config(package = "mypackage", section = "production", 
#'                          inherit = "default", origin = "template")
#'
#' # Use specific template and local files
#' config <- get_config(package = "mypackage", 
#'                     fn_tmpl = "custom_template.yml",
#'                     fn_local = "custom_local.yml")
#' }
#'
#' @export
get_config <- function(package = get_package_name(),
                       origin = "priority",
                       section = "default",
                       fn_tmpl = NULL,
                       fn_local = NULL,
                       case_format = "snake_case",
                       inherit = NULL,
                       verbose = FALSE,
                       validate = TRUE,
                       confirm_fuzzy = TRUE) {

  # Validate origin parameter
  valid_origins <- c("template", "local", "renviron", "priority")
  if (!origin %in% valid_origins) {
    .icy_stop(c(
      paste0("Invalid origin: ", origin),
      "i" = paste0("Valid origins are: ", paste(valid_origins, collapse = ", "))
    ))
  }
  
  # Always resolve file paths for origins that need them
  if (origin %in% c("template", "local", "priority")) {
    # Use find_config_files to get both template and local files
    resolved_files <- find_config_files(
      package = package,
      fn_local = fn_local,
      fn_tmpl = fn_tmpl,
      fuzzy = TRUE,
      confirm_fuzzy = confirm_fuzzy,
      case_format = case_format,
      verbose = verbose
    )
    
    resolved_local_path <- resolved_files$fn_local
    resolved_template_path <- resolved_files$fn_tmpl
  } else {
    # renviron origin doesn't need file resolution
    resolved_local_path <- NULL
    resolved_template_path <- NULL
  }
  
  # Validate configuration file based on origin
  if (validate) {
    if (origin == "template" && !is.null(resolved_template_path)) {
      # Validate template file
      validation <- validate_config_file(
        file_path = resolved_template_path,
        type = "template",
        package = package,
        verbose = FALSE
      )
      
      if (!validation$valid && length(validation$errors) > 0) {
        .icy_stop(c(
          "Template validation failed",
          "x" = validation$errors[1],
          "i" = "Use validate = FALSE to skip validation"
        ))
      }
    } else if (origin == "local" && !is.null(resolved_local_path)) {
      # Validate local config file
      validation <- validate_config_file(
        file_path = resolved_local_path,
        type = "local",
        package = package,
        template_path = resolved_template_path,
        verbose = FALSE
      )
      
      if (!validation$valid && length(validation$errors) > 0) {
        .icy_stop(c(
          "Local config validation failed",
          "x" = validation$errors[1],
          "i" = "Use validate = FALSE to skip validation"
        ))
      }
    }
  }

  # Route to appropriate internal function based on origin
  if (origin == "template") {
    config <- .get_config_template(
      package = package,
      section = section,
      resolved_template_path = resolved_template_path,
      case_format = case_format,
      verbose = verbose
    )
  } else if (origin == "local") {
    config <- .get_config_local(
      package = package,
      section = section,
      resolved_local_path = resolved_local_path,
      case_format = case_format,
      verbose = verbose
    )
  } else if (origin == "renviron") {
    config <- .get_config_renviron(
      package = package,
      section = section,
      resolved_template_path = resolved_template_path,
      case_format = case_format,
      verbose = verbose
    )
  } else if (origin == "priority") {
    config <- .get_config_priority(
      package = package,
      section = section,
      resolved_local_path = resolved_local_path,
      resolved_template_path = resolved_template_path,
      case_format = case_format,
      verbose = verbose
    )
  }

  # Check for explicit no-inheritance directive
  if (!is.null(inherit) && (inherit == 0 || inherit == "0")) {
    if (verbose) {
      .icy_text("Inheritance explicitly disabled")
    }
    return(config)
  }

  # Check for automatic inheritance from appropriate source if inherit is NULL
  if (is.null(inherit) && origin %in% c("template", "local", "priority")) {
    # For local origin, read inheritances from LOCAL config
    # For template/priority, use template's inheritances
    inherit_source <- .get_inherit_config(
      package = package,
      origin = origin,
      resolved_template_path = resolved_template_path,
      resolved_local_path = resolved_local_path,
      case_format = case_format
    )
    
    if (!is.null(inherit_source)) {
      resolved_inherit <- .resolve_inheritance(
        section = section,
        inherit_map = inherit_source,
        verbose = verbose
      )
      
      if (!is.null(resolved_inherit)) {
        inherit <- resolved_inherit
        if (verbose) {
          .icy_text(paste0("Using template-defined inheritance: ", section, " inherits from ", inherit))
        }
      }
    }
  }
  
  # Apply inheritance if requested or auto-detected
  if (!is.null(inherit) && inherit != section && origin %in% c("template", "local", "priority")) {
    if (verbose && !exists("resolved_inherit", inherits = FALSE)) {
      .icy_text(paste0("Applying inheritance from section '", inherit, "' to '", section, "'"))
    }
    
    # Get the base config to inherit from - RECURSIVELY to get full inheritance chain
    # Pass inherit=NULL to let it auto-detect from template but prevent infinite recursion
    # Only pass filenames if user originally specified them (not when using defaults)
    recursive_fn_tmpl <- if (!is.null(fn_tmpl) && !is.null(resolved_template_path)) basename(resolved_template_path) else NULL
    recursive_fn_local <- if (!is.null(fn_local) && !is.null(resolved_local_path)) basename(resolved_local_path) else NULL
    
    base_config <- get_config(
      package = package,
      origin = origin,
      section = inherit,
      fn_tmpl = recursive_fn_tmpl,
      fn_local = recursive_fn_local,
      case_format = case_format,
      inherit = NULL,  # Critical: prevent double-processing of inheritance
      verbose = FALSE
    )
    
    # Apply inheritance using dedicated function
    config <- .apply_inheritance(config, base_config)
  }

  return(config)
}


#' Get configuration from local file
#' 
#' Reads configuration from the user's local YAML file, typically stored in
#' the user's config directory (~/.config/R/package/ or similar).
#' Returns an empty list if the local file doesn't exist.
#' 
#' @keywords internal
.get_config_local <- function(package = get_package_name(),
                              section = "default",
                              resolved_local_path = NULL,
                              case_format = "snake_case",
                              verbose = FALSE) {
  # Use resolved path if provided, otherwise find defaults
  if (is.null(resolved_local_path)) {
    # Find default local config file
    config_files <- find_config_files(
      package = package,
      case_format = case_format,
      verbose = verbose
    )
    config_file_path <- config_files$fn_local

    if (is.null(config_file_path)) {
      return(list()) # Return empty list if no local config exists
    }
  } else {
    config_file_path <- resolved_local_path
  }

  if (verbose) {
    .icy_text(paste0("Reading local config from: ", config_file_path))
  }

  # Read the YAML file
  tryCatch(
    {
      config_data <- yaml::read_yaml(config_file_path)

      # Extract user section
      if (!section %in% names(config_data)) {
        .icy_stop(c(
          paste0("Section ", section, " not found in local config"),
          "i" = paste0("Available sections: ", paste(names(config_data), collapse = ", "))
        ))
      }

      config <- config_data[[section]]

      if (is.null(config) || length(config) == 0) {
        return(list())
      }

      # Resolve variable references and paths
      if (length(config) > 0) {
        # First resolve variable references
        config <- .resolve_variable_references(config)
        
        # Then get types from template for path resolution
        template_types <- tryCatch({
          template_files <- find_config_files(
            package = package,
            fn_tmpl = NULL,
            fn_local = NULL,
            case_format = case_format,
            verbose = FALSE,
            confirm_fuzzy = FALSE
          )
          if (!is.null(template_files$fn_tmpl) && file.exists(template_files$fn_tmpl)) {
            template_data <- yaml::read_yaml(template_files$fn_tmpl)
            if ("types" %in% names(template_data)) {
              template_data$types
            } else {
              list()
            }
          } else {
            list()
          }
        }, error = function(e) list())
        
        # Resolve paths for variables with type="path"
        for (var_name in names(config)) {
          if (!is.null(template_types[[var_name]]) && template_types[[var_name]] == "path") {
            if (is.character(config[[var_name]])) {
              config[[var_name]] <- .resolve_special_path(config[[var_name]], package, config)
            }
          }
        }
      }

      return(config)
    },
    error = function(e) {
      .icy_stop(paste0("Error reading YAML file: ", e$message))
    }
  )
}


#' Get configuration from template file
#' 
#' Reads configuration from the package's template YAML file, typically stored
#' in the package's inst/ directory. This provides the default configuration
#' blueprint that users can customize via local config files.
#' 
#' @keywords internal
.get_config_template <- function(package = get_package_name(),
                                 section = "default",
                                 resolved_template_path = NULL,
                                 case_format = "snake_case",
                                 verbose = FALSE) {
  # Use resolved path if provided, otherwise find defaults
  if (is.null(resolved_template_path)) {
    # Find default template config file
    config_files <- find_config_files(
      package = package,
      case_format = case_format
    )
    config_file_path <- config_files$fn_tmpl

    if (is.null(config_file_path)) {
      .icy_stop(paste0("No template configuration file found for package ", package))
    }
  } else {
    config_file_path <- resolved_template_path
  }

  if (verbose) {
    .icy_text(paste0("Reading template config from: ", config_file_path))
  }

  # Read the YAML file
  tryCatch(
    {
      config_data <- yaml::read_yaml(config_file_path)

      # Extract user section
      if (!section %in% names(config_data)) {
        .icy_stop(c(
          paste0("Section ", section, " not found in template"),
          "i" = paste0("Available sections: ", paste(names(config_data), collapse = ", "))
        ))
      }

      config <- config_data[[section]]

      if (is.null(config) || length(config) == 0) {
        return(list())  # Return empty list, same as .get_config_local()
      }

      # Resolve variable references and path variables using template types
      if (length(config) > 0) {
        # First resolve variable references
        config <- .resolve_variable_references(config)
        
        # Then resolve path variables if types are defined
        if ("types" %in% names(config_data)) {
          template_types <- config_data$types
          
          for (var_name in names(config)) {
            if (!is.null(template_types[[var_name]]) && template_types[[var_name]] == "path") {
              if (is.character(config[[var_name]])) {
                config[[var_name]] <- .resolve_special_path(config[[var_name]], package, config)
              }
            }
          }
        }
      }

      return(config)
    },
    error = function(e) {
      .icy_stop(paste0("Error reading template YAML file: ", e$message))
    }
  )
}


#' Get configuration from .Renviron file
#' 
#' Reads configuration from the user's .Renviron file, filtering for variables
#' that match the package's template. Only returns environment variables that
#' are defined in the template to avoid including unrelated variables.
#' 
#' @keywords internal
.get_config_renviron <- function(package = get_package_name(),
                                 section = "default",
                                 resolved_template_path = NULL,
                                 case_format = "snake_case",
                                 verbose = FALSE) {
  # Get path to .Renviron
  renviron_path <- path.expand("~/.Renviron")

  if (!file.exists(renviron_path)) {
    return(list()) # Return empty list if no .Renviron exists
  }

  # Read .Renviron file
  lines <- readLines(renviron_path, warn = FALSE)

  # Parse environment variables
  env_vars <- list()

  for (line in lines) {
    # Skip empty lines and comments
    if (nchar(trimws(line)) == 0 || grepl("^\\s*#", line)) {
      next
    }

    # Parse variable assignments (VAR=value or VAR="value")
    if (grepl("^\\s*[A-Za-z_][A-Za-z0-9_]*\\s*=", line)) {
      # Split on first = sign
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        var_name <- trimws(parts[1])
        var_value <- paste(parts[-1], collapse = "=") # In case value contains =
        var_value <- trimws(var_value)

        # Remove quotes if present
        if (grepl('^".*"$', var_value) || grepl("^'.*'$", var_value)) {
          var_value <- substr(var_value, 2, nchar(var_value) - 1)
        }

        env_vars[[var_name]] <- var_value
      }
    }
  }

  if (verbose) {
    .icy_text(paste0("Reading .Renviron: ", renviron_path))
  }

  # If package is specified, filter to only package-specific variables
  if (!is.null(package)) {
    # First, try to get variable names from template
    template_result <- tryCatch(
      {
        .get_config_template(package = package,
                             section = section,
                             resolved_template_path = resolved_template_path,
                             case_format = case_format,
                             verbose = FALSE)
      },
      error = function(e) NULL
    )

    if (!is.null(template_result)) {
      # Template was successfully read - filter based on its variables
      template_vars <- names(template_result)
      if (is.null(template_vars)) {
        template_vars <- character(0)  # Empty section should have no variables
      }
      env_vars <- env_vars[intersect(names(env_vars), template_vars)]
    }
    # If template_result is NULL (template read failed), return all variables as fallback
  }

  return(env_vars)
}


#' Get configuration with priority resolution
#' 
#' Merges configurations from all sources following the priority hierarchy:
#' Template (lowest) -> Local -> .Renviron -> Session (highest).
#' Template values serve as defaults that can be overridden by higher priority sources.
#' 
#' @param package Package name
#' @param section Section in YAML file
#' @param resolved_local_path Pre-resolved local file path
#' @param resolved_template_path Pre-resolved template file path
#' @param case_format Case format for file searching
#' @param verbose Show informative messages
#' @return Named list with merged configuration following priority order
#' @keywords internal
.get_config_priority <- function(package = get_package_name(),
                                 section = "default",
                                 resolved_local_path = NULL,
                                 resolved_template_path = NULL,
                                 case_format = "snake_case",
                                 verbose = FALSE) {
  # Get configurations from all sources (template -> local -> .Renviron -> session)
  template_config <- .get_config_template(
    package = package,
    section = section,
    resolved_template_path = resolved_template_path,
    case_format = case_format,
    verbose = verbose
  )
  
  local_config <- .get_config_local(
    package = package,
    section = section,
    resolved_local_path = resolved_local_path,
    case_format = case_format,
    verbose = verbose
  )

  renviron_config <- .get_config_renviron(
    package = package,
    section = section,
    resolved_template_path = resolved_template_path,
    case_format = case_format,
    verbose = verbose
  )

  # Start with template as base (lowest priority)
  config <- template_config

  # Override with local config values
  for (var_name in names(local_config)) {
    config[[var_name]] <- local_config[[var_name]]
  }
  
  # Override with .Renviron values
  for (var_name in names(renviron_config)) {
    config[[var_name]] <- renviron_config[[var_name]]
  }

  # Override with session environment variables (highest priority)
  for (var_name in names(config)) {
    session_value <- Sys.getenv(var_name, unset = NA)
    if (!is.na(session_value)) {
      # Convert session string back to appropriate type based on config file type
      original_value <- config[[var_name]]
      
      if (is.logical(original_value)) {
        # Convert to logical
        config[[var_name]] <- .convert_by_type(session_value, "logical")
      } else if (is.numeric(original_value)) {
        # Convert to numeric
        converted <- suppressWarnings(as.numeric(session_value))
        config[[var_name]] <- if (is.na(converted)) session_value else converted
      } else {
        # Keep as character
        config[[var_name]] <- session_value
      }
    }
  }

  # Resolve variable references FIRST (before path resolution)
  # This allows ${VAR_NAME} references to be resolved before path keywords
  if (length(config) > 0) {
    config <- .resolve_variable_references(config)
  }
  
  # Final path resolution step - resolve any path variables that might have been
  # overridden by .Renviron or session (which would be unresolved strings)
  if (length(config) > 0) {
    # Get types from template to identify path variables
    template_types <- tryCatch({
      if (!is.null(resolved_template_path) && file.exists(resolved_template_path)) {
        template_data <- yaml::read_yaml(resolved_template_path)
        if ("types" %in% names(template_data)) {
          template_data$types
        } else {
          list()
        }
      } else {
        # Try to find template if path not provided
        template_files <- find_config_files(
          package = package,
          fn_tmpl = NULL,
          fn_local = NULL,
          case_format = case_format,
          verbose = FALSE,
          confirm_fuzzy = FALSE
        )
        if (!is.null(template_files$fn_tmpl) && file.exists(template_files$fn_tmpl)) {
          template_data <- yaml::read_yaml(template_files$fn_tmpl)
          if ("types" %in% names(template_data)) {
            template_data$types
          } else {
            list()
          }
        } else {
          list()
        }
      }
    }, error = function(e) list())
    
    # Resolve paths for variables with type="path"
    # Pass the resolved config so path resolution can also use variable references
    for (var_name in names(config)) {
      if (!is.null(template_types[[var_name]]) && template_types[[var_name]] == "path") {
        if (is.character(config[[var_name]])) {
          config[[var_name]] <- .resolve_special_path(config[[var_name]], package, config)
        }
      }
    }
  }

  return(config)
}




#' Get inheritance configuration from appropriate source
#'
#' Reads the inheritances section from either template or local config based on origin.
#' For local origin, reads from local config. For template/priority, reads from template.
#'
#' @param package Package name
#' @param origin Origin to determine which file to read ("template", "local", "priority")
#' @param resolved_template_path Optional path to resolved template YAML file
#' @param resolved_local_path Optional path to resolved local YAML file
#' @param case_format Case format for file searching
#' @return Named list mapping sections to their parent sections, or NULL if no inheritances section
#' @keywords internal
.get_inherit_config <- function(package, 
                               origin = "template",
                               resolved_template_path = NULL, 
                               resolved_local_path = NULL,
                               case_format = "snake_case") {
  
  # Determine which file to read based on origin
  if (origin == "local" && !is.null(resolved_local_path)) {
    # For local origin, read inheritances from LOCAL config
    file_to_read <- resolved_local_path
  } else if (origin == "local") {
    # Need to find local file
    config_files <- find_config_files(
      package = package,
      case_format = case_format
    )
    file_to_read <- config_files$fn_local
    
    if (is.null(file_to_read)) {
      return(NULL)  # No local config, no inheritance
    }
  } else if (!is.null(resolved_template_path)) {
    # For template/priority origin with resolved path
    file_to_read <- resolved_template_path
  } else {
    # Need to find template file
    config_files <- find_config_files(
      package = package,
      case_format = case_format
    )
    file_to_read <- config_files$fn_tmpl
    
    if (is.null(file_to_read)) {
      return(NULL)  # No template, no inheritance
    }
  }
  
  # Read the determined file and extract inheritances section
  tryCatch({
    config_data <- yaml::read_yaml(file_to_read)
    
    if ("inheritances" %in% names(config_data)) {
      return(config_data$inheritances)
    }
    
    return(NULL)
  }, error = function(e) {
    return(NULL)  # Error reading file, no inheritance
  })
}


#' Resolve inheritance chain for a section
#'
#' Resolves the inheritance chain for a given section, handling recursive
#' inheritance and circular dependency detection.
#'
#' @param section The section to resolve inheritance for
#' @param inherit_map Named list mapping sections to their parents
#' @param max_depth Maximum depth for inheritance chain (prevents infinite loops)
#' @param verbose Show messages
#' @return The section to inherit from, or NULL if no inheritance
#' @keywords internal
.resolve_inheritance <- function(section, inherit_map, max_depth = 10, verbose = FALSE) {
  if (is.null(inherit_map) || !is.list(inherit_map)) {
    return(NULL)
  }
  
  # Check if section has inheritance defined
  if (!(section %in% names(inherit_map))) {
    return(NULL)
  }
  
  inherit_from <- inherit_map[[section]]
  
  # Handle NULL or ~ in YAML (no inheritance)
  if (is.null(inherit_from)) {
    return(NULL)
  }
  
  # Track visited sections to detect circular dependencies
  visited <- c(section)
  current <- inherit_from
  depth <- 1
  
  # Follow the inheritance chain
  while (!is.null(current) && current %in% names(inherit_map) && depth < max_depth) {
    if (current %in% visited) {
      if (verbose) {
        .icy_warn(paste0("Circular inheritance detected: ", 
                        paste(c(visited, current), collapse = " -> ")))
      }
      return(inherit_from)  # Return the direct parent only
    }
    
    visited <- c(visited, current)
    next_inherit <- inherit_map[[current]]
    
    if (is.null(next_inherit)) {
      # Reached the end of chain
      return(inherit_from)  # Return the direct parent
    }
    
    current <- next_inherit
    depth <- depth + 1
  }
  
  if (depth >= max_depth && verbose) {
    .icy_warn(paste0("Maximum inheritance depth reached for section: ", section))
  }
  
  return(inherit_from)
}


#' Apply configuration inheritance
#'
#' Merges two configurations with the child config values overriding parent values.
#' Properly handles NULL values.
#'
#' @param config The child configuration
#' @param base_config The parent configuration to inherit from
#' @return Merged configuration with child values taking precedence
#' @keywords internal
.apply_inheritance <- function(config, base_config) {
  if (is.null(base_config) || length(base_config) == 0) {
    return(config)
  }
  
  if (is.null(config) || length(config) == 0) {
    return(base_config)
  }
  
  # Merge configs: current config values override base config
  # Only add keys from base that don't exist in current config
  # We need to preserve NULL values, so we'll build a new list
  merged_config <- config
  
  for (key in names(base_config)) {
    if (!(key %in% names(config))) {
      # Use single bracket assignment to preserve NULL values
      merged_config[key] <- base_config[key]
    }
  }
  
  return(merged_config)
}

