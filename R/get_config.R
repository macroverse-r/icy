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
#' @param yaml_file Character string with the name or path to the YAML file. If NULL,
#'   the function will search for the appropriate file based on the origin.
#' @param case_format Character string indicating the case format to use for
#'   searching the YAML file if `yaml_file` is NULL. Options are:
#'   "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
#' @param inherit Character string specifying a section to inherit values from.
#'   If NULL (default), no inheritance is applied. When specified, values from the
#'   inherit section are used as defaults, which can be overridden by the main section.
#'   Only works with "template" and "local" origins.
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
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
#' }
#'
#' @export
get_config <- function(package = get_package_name(),
                       origin = "priority",
                       section = "default",
                       yaml_file = NULL,
                       case_format = "snake_case",
                       inherit = NULL,
                       verbose = FALSE) {

  # Validate origin parameter
  valid_origins <- c("template", "local", "renviron", "priority")
  if (!origin %in% valid_origins) {
    .icy_stop(c(
      paste0("Invalid origin: ", origin),
      "i" = paste0("Valid origins are: ", paste(valid_origins, collapse = ", "))
    ))
  }

  # Route to appropriate internal function based on origin
  if (origin == "template") {
    config <- .get_config_template(
      package = package,
      section = section,
      yaml_file = yaml_file,
      case_format = case_format,
      verbose = verbose
    )
  } else if (origin == "local") {
    config <- .get_config_local(
      package = package,
      section = section,
      yaml_file = yaml_file,
      case_format = case_format,
      verbose = verbose
    )
  } else if (origin == "renviron") {
    config <- .get_config_renviron(
      package = package,
      section = section,
      verbose = verbose
    )
  } else if (origin == "priority") {
    config <- .get_config_priority(
      package = package,
      section = section,
      yaml_file = yaml_file,
      case_format = case_format,
      verbose = verbose
    )
  }

  # Apply inheritance if requested
  if (!is.null(inherit) && inherit != section && origin %in% c("template", "local")) {
    if (verbose) {
      .icy_text(paste0("Applying inheritance from section '", inherit, "' to '", section, "'"))
    }
    
    # Get the base config to inherit from
    base_config <- if (origin == "template") {
      .get_config_template(
        package = package,
        section = inherit,
        yaml_file = yaml_file,
        case_format = case_format,
        verbose = FALSE
      )
    } else {
      .get_config_local(
        package = package,
        section = inherit,
        yaml_file = yaml_file,
        case_format = case_format,
        verbose = FALSE
      )
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
    config <- merged_config
  }

  return(config)
}


#' Get configuration from local file
#' @keywords internal
.get_config_local <- function(package = get_package_name(),
                              section = "default",
                              yaml_file = NULL,
                              case_format = "snake_case",
                              verbose = FALSE) {
  # Find the YAML file
  if (is.null(yaml_file)) {
    yaml_file <- find_local(
      package = package,
      case_format = case_format,
      verbose = verbose
    )

    if (is.null(yaml_file)) {
      return(list()) # Return empty list if no local config exists
    }
  } else {
    # If yaml_file is provided, use pattern matching
    if (!file.exists(yaml_file)) {
      # Try pattern matching
      matching_files <- .find_matching_pattern(
        package = package,
        fn_pattern = yaml_file,
        user_dir = TRUE,
        verbose = verbose
      )
      
      if (length(matching_files) == 0) {
        .icy_stop(paste0("Local YAML file not found: ", yaml_file))
      } else if (length(matching_files) > 1) {
        .icy_warn(paste0("Multiple files found matching '", yaml_file, "'. Using: ", matching_files[1]))
        yaml_file <- matching_files[1]
      } else {
        yaml_file <- matching_files[1]
      }
    }
  }

  if (verbose) {
    .icy_text(paste0("Reading local config from: ", yaml_file))
  }

  # Read the YAML file
  tryCatch(
    {
      config_data <- yaml::read_yaml(yaml_file)

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
                                 yaml_file = NULL,
                                 case_format = "snake_case",
                                 verbose = FALSE) {
  # Find the YAML file
  if (is.null(yaml_file)) {
    yaml_file <- find_template(
      package = package,
      case_format = case_format
    )

    if (is.null(yaml_file)) {
      .icy_stop(paste0("No template configuration file found for package ", package))
    }
  } else {
    # If yaml_file is provided, use pattern matching
    if (!file.exists(yaml_file)) {
      # Try pattern matching
      matching_files <- .find_matching_pattern(
        package = package,
        fn_pattern = yaml_file,
        user_dir = FALSE,  # Templates are in package installation dir
        verbose = verbose
      )
      
      if (length(matching_files) == 0) {
        .icy_stop(paste0("Template YAML file not found: ", yaml_file))
      } else if (length(matching_files) > 1) {
        .icy_warn(paste0("Multiple files found matching '", yaml_file, "'. Using: ", matching_files[1]))
        yaml_file <- matching_files[1]
      } else {
        yaml_file <- matching_files[1]
      }
    }
  }

  if (verbose) {
    .icy_text(paste0("Reading template config from: ", yaml_file))
  }

  # Read the YAML file
  tryCatch(
    {
      config_data <- yaml::read_yaml(yaml_file)

      # Extract user section
      if (!section %in% names(config_data)) {
        .icy_stop(c(
          paste0("Section ", section, " not found in template"),
          "i" = paste0("Available sections: ", paste(names(config_data), collapse = ", "))
        ))
      }

      config <- config_data[[section]]

      if (is.null(config) || length(config) == 0) {
        .icy_stop(paste0("No environment variables found in template section ", section))
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
    template_vars <- tryCatch(
      {
        names(get_config(package = package,
                         origin = "template",
                         section = section))
      },
      error = function(e) NULL
    )

    if (!is.null(template_vars)) {
      # Filter to only variables defined in template
      env_vars <- env_vars[intersect(names(env_vars), template_vars)]
    }
  }

  return(env_vars)
}


#' Get configuration with priority resolution
#' @keywords internal
.get_config_priority <- function(package = get_package_name(),
                                 section = "default",
                                 yaml_file = NULL,
                                 case_format = "snake_case",
                                 verbose = FALSE) {
  # Get configurations from both sources
  local_config <- .get_config_local(
    package = package,
    section = section,
    yaml_file = yaml_file,
    case_format = case_format,
    verbose = verbose
  )

  renviron_config <- .get_config_renviron(
    package = package,
    section = section,
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
        config[[var_name]] <- .convert_to_logical(session_value)
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


#' Convert string to logical value
#' @keywords internal
.convert_to_logical <- function(value) {
  if (is.na(value) || value == "") {
    return(NA)
  }
  
  lower_value <- tolower(trimws(value))
  
  if (lower_value %in% c("true", "t", "yes", "y", "1")) {
    return(TRUE)
  } else if (lower_value %in% c("false", "f", "no", "n", "0")) {
    return(FALSE)
  } else {
    # Return the original string if we can't convert
    return(value)
  }
}

