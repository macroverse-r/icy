#' Inheritance Utilities
#'
#' Internal functions for resolving and applying section inheritance
#' in configuration files. Used by both get_config() and get_template().
#'
#' @name inheritance-utils
#' @keywords internal
NULL


#' Resolve and apply inheritance from pre-parsed YAML data
#'
#' Handles the full inheritance workflow: extracts the inheritance map,
#' auto-detects or validates the inheritance directive, builds the chain,
#' and merges ancestor sections into the config.
#'
#' @param config The config list for the target section (already extracted)
#' @param raw_data The full pre-parsed YAML data (all sections)
#' @param section The target section name
#' @param inherit Explicit inherit directive (NULL for auto-detect, 0 to disable)
#' @param type "local" or "template" (for source_label in error messages)
#' @param template_types Pre-extracted template types
#' @param package Package name
#' @param verbose Logical
#' @return Config list with inheritance applied
#' @keywords internal
.apply_section_inheritance <- function(config, raw_data, section, inherit,
                                      type, template_types,
                                      package, verbose = FALSE) {
  # Explicit disable
  if (!is.null(inherit) && (inherit == 0 || inherit == "0")) {
    if (verbose) .icy_text("Inheritance explicitly disabled")
    return(config)
  }

  # Extract inheritance map from pre-parsed data
  inherit_map <- if (!is.null(raw_data) && "inheritances" %in% names(raw_data)) {
    raw_data$inheritances
  }

  # Auto-detect inheritance if not explicitly specified
  if (is.null(inherit) && !is.null(inherit_map)) {
    resolved_inherit <- .resolve_inheritance(
      section = section,
      inherit_map = inherit_map,
      verbose = verbose
    )

    if (!is.null(resolved_inherit)) {
      inherit <- resolved_inherit
      if (verbose) {
        .icy_text(paste0("Using defined inheritance: ", section, " inherits from ", inherit))
      }
    }
  }

  # Apply inheritance iteratively
  if (!is.null(inherit) && inherit != section) {
    if (verbose) {
      .icy_text(paste0("Applying inheritance from section '", inherit, "' to '", section, "'"))
    }

    # Build full inheritance chain
    chain <- character()
    current <- inherit
    visited <- section
    while (!is.null(current) && !current %in% visited) {
      chain <- c(chain, current)
      visited <- c(visited, current)
      if (!is.null(inherit_map) && current %in% names(inherit_map)) {
        current <- inherit_map[[current]]
      } else {
        break
      }
    }

    # Merge from deepest ancestor to closest parent
    base_config <- list()
    for (ancestor in rev(chain)) {
      ancestor_config <- .process_config_section(
        config_data = raw_data,
        section = ancestor,
        template_types = template_types,
        package = package,
        source_label = type
      )
      base_config <- .apply_inheritance(ancestor_config, base_config)
    }

    config <- .apply_inheritance(config, base_config)
  }

  return(config)
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
