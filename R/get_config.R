#' Get Environment Variable Configuration
#'
#' Reads environment variable configuration from different origins (template, local, or .Renviron).
#' This function is typically used within an R package to retrieve environment variable
#' configurations based on the specified origin.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param origin Character string specifying where to read the configuration from:
#'   - "template": Read from the package's template YAML file (read-only blueprint)
#'   - "local": Read from the user's local configuration file (default)
#'   - "renviron": Read from .Renviron file
#'   - "priority": Read with priority order (.Renviron > local config)
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
#' # Get configuration from template
#' template_config <- get_config(package = "mypackage", origin = "template")
#'
#' # Get configuration from local file
#' local_config <- get_config(package = "mypackage", origin = "local")
#'
#' # Get configuration with priority resolution
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
  
  # Resolve file paths once at the beginning
  if (!is.null(fn_tmpl) || !is.null(fn_local)) {
    # Use find_config_files to get both files
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
    # No specific file provided, will find defaults
    resolved_local_path <- NULL
    resolved_template_path <- NULL
  }
  
  # Validate template if enabled and using template/local origin
  if (origin %in% c("template", "local") && validate && !is.null(resolved_template_path)) {
    # Quick validation for performance
    validation <- validate_template(
      package = package,
      fn_tmpl = basename(resolved_template_path),  # Use resolved path
      case_format = case_format,
      verbose = FALSE,
      quick = TRUE
    )
    
    if (!validation$valid && length(validation$errors) > 0) {
      .icy_stop(c(
        "Template validation failed",
        "x" = validation$errors[1],
        "i" = "Use validate = FALSE to skip validation"
      ))
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

  # Check for automatic inheritance from template if inherit is NULL
  if (is.null(inherit) && origin %in% c("template", "local", "priority")) {
    template_inherit <- .get_inherit_config(
      package = package,
      resolved_template_path = resolved_template_path,
      case_format = case_format
    )
    
    if (!is.null(template_inherit)) {
      resolved_inherit <- .resolve_inheritance(
        section = section,
        inherit_map = template_inherit,
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

      return(config)
    },
    error = function(e) {
      .icy_stop(paste0("Error reading YAML file: ", e$message))
    }
  )
}


#' Get configuration from template file
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

      return(config)
    },
    error = function(e) {
      .icy_stop(paste0("Error reading template YAML file: ", e$message))
    }
  )
}


#' Get configuration from .Renviron file
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
#' @keywords internal
.get_config_priority <- function(package = get_package_name(),
                                 section = "default",
                                 resolved_local_path = NULL,
                                 resolved_template_path = NULL,
                                 case_format = "snake_case",
                                 verbose = FALSE) {
  # Get configurations from both sources
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

  # Merge with .Renviron taking priority
  config <- local_config

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

  return(config)
}




#' Get inheritance configuration from template
#'
#' Reads the inheritances section from a template YAML file if it exists.
#'
#' @param package Package name
#' @param resolved_template_path Optional path to resolved template YAML file
#' @param case_format Case format for file searching
#' @return Named list mapping sections to their parent sections, or NULL if no inheritances section
#' @keywords internal
.get_inherit_config <- function(package, resolved_template_path = NULL, case_format = "snake_case") {
  # Use resolved path if provided, otherwise find defaults
  if (is.null(resolved_template_path)) {
    config_files <- find_config_files(
      package = package,
      case_format = case_format
    )
    template_file <- config_files$fn_tmpl
    
    if (is.null(template_file)) {
      return(NULL)  # No template, no inheritance
    }
  } else {
    template_file <- resolved_template_path
  }
  
  # Read template and extract inherit section
  tryCatch({
    template_data <- yaml::read_yaml(template_file)
    
    if ("inheritances" %in% names(template_data)) {
      return(template_data$inheritances)
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

