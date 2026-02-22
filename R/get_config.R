#' Get Configuration
#'
#' Reads configuration from the local YAML file (single source of truth) or
#' from the template file (blueprint inspection). The local config file is
#' the authoritative source for all configuration values.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param origin Character string specifying where to read the configuration from:
#'   - "local": Read from the user's local configuration file (default, single source of truth)
#'   - "template": Read from the package's template YAML file (read-only blueprint)
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
#'   for an "inheritances" section in the config that defines automatic inheritance
#'   relationships. When explicitly specified, this parameter overrides any
#'   defined inheritance. Use 0 to disable inheritance even if defined.
#'   Values from the inherit section are used as defaults, which can
#'   be overridden by the main section. Inheritance is recursive - if A inherits
#'   from B and B inherits from C, A will receive values from both B and C.
#' @param verbose Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.
#' @param validate Logical. If TRUE (default), validates the configuration file
#'   structure. Set to FALSE to skip validation for performance.
#' @param confirm_fuzzy Logical. If TRUE (default), prompts for confirmation when fuzzy
#'   matches are used in file searching. Set to FALSE for non-interactive use.
#'
#' @return Named list of configuration values.
#'
#' @examples
#' \dontrun{
#' # Get configuration from local file (default, single source of truth)
#' config <- get_config(package = "mypackage")
#'
#' # Get configuration from template (package defaults/blueprint)
#' template_config <- get_config(package = "mypackage", origin = "template")
#'
#' # Get production config with defaults inherited from default section
#' prod_config <- get_config(package = "mypackage", section = "production",
#'                          inherit = "default")
#' }
#'
#' @export
get_config <- function(package = get_package_name(),
                       origin = "local",
                       section = "default",
                       fn_tmpl = NULL,
                       fn_local = NULL,
                       case_format = "snake_case",
                       inherit = NULL,
                       verbose = FALSE,
                       validate = TRUE,
                       confirm_fuzzy = TRUE,
                       .check_conflicts = TRUE) {

  # Validate origin parameter
  valid_origins <- c("template", "local")
  if (!origin %in% valid_origins) {
    .icy_stop(c(
      paste0("Invalid origin: ", origin),
      "i" = paste0("Valid origins are: ", paste(valid_origins, collapse = ", "))
    ))
  }

  # Resolve file paths
  resolved_files <- .find_config_files(
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

  # Validate configuration file based on origin
  if (validate) {
    if (origin == "template" && !is.null(resolved_template_path)) {
      validation <- validate_config_file(
        fn_tmpl = resolved_template_path,
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
      validation <- validate_config_file(
        fn_local = resolved_local_path,
        fn_tmpl = resolved_template_path,
        type = "local",
        package = package,
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
  } else {
    # origin == "local" (single source of truth)
    config <- .read_local_yaml(
      package = package,
      section = section,
      resolved_local_path = resolved_local_path,
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

  # Check for automatic inheritance if inherit is NULL
  if (is.null(inherit) && origin %in% c("template", "local")) {
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
          .icy_text(paste0("Using defined inheritance: ", section, " inherits from ", inherit))
        }
      }
    }
  }

  # Apply inheritance if requested or auto-detected
  if (!is.null(inherit) && inherit != section && origin %in% c("template", "local")) {
    if (verbose && !exists("resolved_inherit", inherits = FALSE)) {
      .icy_text(paste0("Applying inheritance from section '", inherit, "' to '", section, "'"))
    }

    recursive_fn_tmpl <- if (!is.null(fn_tmpl) && !is.null(resolved_template_path)) basename(resolved_template_path) else NULL
    recursive_fn_local <- if (!is.null(fn_local) && !is.null(resolved_local_path)) basename(resolved_local_path) else NULL

    base_config <- get_config(
      package = package,
      origin = origin,
      section = inherit,
      fn_tmpl = recursive_fn_tmpl,
      fn_local = recursive_fn_local,
      case_format = case_format,
      inherit = NULL,
      verbose = FALSE,
      .check_conflicts = FALSE
    )

    config <- .apply_inheritance(config, base_config)
  }

  # Check for conflicts between local config and session environment (once, after inheritance)
  if (origin == "local" && .check_conflicts) {
    check_conflicts(
      package = package,
      mode = "warn",
      config = config
    )
  }

  return(config)
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
    config_files <- .find_config_files(
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

  tryCatch(
    {
      config_data <- yaml::read_yaml(config_file_path)

      if (!section %in% names(config_data)) {
        .icy_stop(c(
          paste0("Section ", section, " not found in template"),
          "i" = paste0("Available sections: ", paste(names(config_data), collapse = ", "))
        ))
      }

      config <- config_data[[section]]

      if (is.null(config) || length(config) == 0) {
        return(list())
      }

      if (length(config) > 0) {
        config <- .resolve_variable_references(config)

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


#' Get inheritance configuration from appropriate source
#'
#' Reads the inheritances section from either template or local config based on origin.
#'
#' @param package Package name
#' @param origin Origin to determine which file to read ("template" or "local")
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
    file_to_read <- resolved_local_path
  } else if (origin == "local") {
    config_files <- .find_config_files(
      package = package,
      case_format = case_format
    )
    file_to_read <- config_files$fn_local

    if (is.null(file_to_read)) {
      return(NULL)
    }
  } else if (!is.null(resolved_template_path)) {
    file_to_read <- resolved_template_path
  } else {
    config_files <- .find_config_files(
      package = package,
      case_format = case_format
    )
    file_to_read <- config_files$fn_tmpl

    if (is.null(file_to_read)) {
      return(NULL)
    }
  }

  tryCatch({
    config_data <- yaml::read_yaml(file_to_read)

    if ("inheritances" %in% names(config_data)) {
      return(config_data$inheritances)
    }

    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}


#' Resolve inheritance chain for a section
#'
#' @param section The section to resolve inheritance for
#' @param inherit_map Named list mapping sections to their parents
#' @param max_depth Maximum depth for inheritance chain
#' @param verbose Show messages
#' @return The section to inherit from, or NULL if no inheritance
#' @keywords internal
.resolve_inheritance <- function(section, inherit_map, max_depth = 10, verbose = FALSE) {
  if (is.null(inherit_map) || !is.list(inherit_map)) {
    return(NULL)
  }

  if (!(section %in% names(inherit_map))) {
    return(NULL)
  }

  inherit_from <- inherit_map[[section]]

  if (is.null(inherit_from)) {
    return(NULL)
  }

  visited <- c(section)
  current <- inherit_from
  depth <- 1

  while (!is.null(current) && current %in% names(inherit_map) && depth < max_depth) {
    if (current %in% visited) {
      if (verbose) {
        .icy_warn(paste0("Circular inheritance detected: ",
                        paste(c(visited, current), collapse = " -> ")))
      }
      return(inherit_from)
    }

    visited <- c(visited, current)
    next_inherit <- inherit_map[[current]]

    if (is.null(next_inherit)) {
      return(inherit_from)
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

  merged_config <- config

  for (key in names(base_config)) {
    if (!(key %in% names(config))) {
      merged_config[key] <- base_config[key]
    }
  }

  return(merged_config)
}
