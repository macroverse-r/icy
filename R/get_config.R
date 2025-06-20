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
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param yaml_file Character string with the name or path to the YAML file. If NULL,
#'   the function will search for the appropriate file based on the origin.
#' @param case_format Character string indicating the case format to use for
#'   searching the YAML file if `yaml_file` is NULL. Options are:
#'   "snake_case" (default), "camelCase", "PascalCase", "kebab-case".
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
#' }
#'
#' @export
get_config <- function(package = get_package_name(),
                       origin = "local",
                       user = "default",
                       yaml_file = NULL,
                       case_format = "snake_case",
                       verbose = FALSE) {

  # Validate origin parameter
  valid_origins <- c("template", "local", "renviron", "priority")
  if (!origin %in% valid_origins) {
    icy_abort(c(
      paste0("Invalid origin: ", origin),
      "i" = paste0("Valid origins are: ", paste(valid_origins, collapse = ", "))
    ))
  }

  # Route to appropriate internal function based on origin
  if (origin == "template") {
    config <- .get_config_template(
      package = package,
      user = user,
      yaml_file = yaml_file,
      case_format = case_format,
      verbose = verbose
    )
  } else if (origin == "local") {
    config <- .get_config_local(
      package = package,
      user = user,
      yaml_file = yaml_file,
      case_format = case_format,
      verbose = verbose
    )
  } else if (origin == "renviron") {
    config <- .get_config_renviron(
      package = package,
      user = user,
      verbose = verbose
    )
  } else if (origin == "priority") {
    config <- .get_config_priority(
      package = package,
      user = user,
      yaml_file = yaml_file,
      case_format = case_format,
      verbose = verbose
    )
  }

  return(config)
}


#' Get configuration from local file
#' @keywords internal
.get_config_local <- function(package = get_package_name(),
                              user = "default",
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
    # If yaml_file is provided, check if it exists
    if (!file.exists(yaml_file)) {
      if (!grepl("[/\\\\]", yaml_file)) {
        # Just a filename, try to find it in package
        yaml_file <- file.path(get_package_path(package = package), yaml_file)
      }
      if (!file.exists(yaml_file)) {
        icy_abort(paste0("YAML file not found: ", yaml_file))
      }
    }
  }

  if (verbose) {
    icy_text(paste0("Reading local config from: ", yaml_file))
  }

  # Read the YAML file
  tryCatch(
    {
      config_data <- yaml::read_yaml(yaml_file)

      # Extract user section
      if (!user %in% names(config_data)) {
        icy_abort(c(
          paste0("User section ", user, " not found in local config"),
          "i" = paste0("Available sections: ", paste(names(config_data), collapse = ", "))
        ))
      }

      config <- config_data[[user]]

      if (is.null(config) || length(config) == 0) {
        return(list())
      }

      return(config)
    },
    error = function(e) {
      icy_abort(paste0("Error reading YAML file: ", e$message))
    }
  )
}


#' Get configuration from template file
#' @keywords internal
.get_config_template <- function(package = get_package_name(),
                                 user = "default",
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
      icy_abort(paste0("No template configuration file found for package ", package))
    }
  } else {
    # If yaml_file is provided, check if it exists
    if (!file.exists(yaml_file)) {
      if (!grepl("[/\\\\]", yaml_file)) {
        # Just a filename, try to find it in package
        yaml_file <- file.path(get_package_path(package = package, user_dir = FALSE), yaml_file)
      }
      if (!file.exists(yaml_file)) {
        icy_abort(paste0("Template YAML file not found: ", yaml_file))
      }
    }
  }

  if (verbose) {
    icy_text(paste0("Reading template config from: ", yaml_file))
  }

  # Read the YAML file
  tryCatch(
    {
      config_data <- yaml::read_yaml(yaml_file)

      # Extract user section
      if (!user %in% names(config_data)) {
        icy_abort(c(
          paste0("User section ", user, " not found in template"),
          "i" = paste0("Available sections: ", paste(names(config_data), collapse = ", "))
        ))
      }

      config <- config_data[[user]]

      if (is.null(config) || length(config) == 0) {
        icy_abort(paste0("No environment variables found in template section ", user))
      }

      return(config)
    },
    error = function(e) {
      icy_abort(paste0("Error reading template YAML file: ", e$message))
    }
  )
}


#' Get configuration from .Renviron file
#' @keywords internal
.get_config_renviron <- function(package = get_package_name(),
                                 user = "default",
                                 verbose = FALSE) {
  # Get path to .Renviron
  renviron_path <- get_renviron_path()

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
    icy_text(paste0("Reading .Renviron: ", renviron_path))
  }

  # If package is specified, filter to only package-specific variables
  if (!is.null(package)) {
    # First, try to get variable names from template
    template_vars <- tryCatch(
      {
        names(get_config(package = package,
                         origin = "template",
                         user = user))
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
                                 user = "default",
                                 yaml_file = NULL,
                                 case_format = "snake_case",
                                 verbose = FALSE) {
  # Get configurations from both sources
  local_config <- .get_config_local(
    package = package,
    user = user,
    yaml_file = yaml_file,
    case_format = case_format,
    verbose = verbose
  )

  renviron_config <- .get_config_renviron(
    package = package,
    user = user,
    verbose = verbose
  )

  # Merge with .Renviron taking priority
  config <- local_config

  # Override with .Renviron values
  for (var_name in names(renviron_config)) {
    config[[var_name]] <- renviron_config[[var_name]]
  }

  return(config)
}

