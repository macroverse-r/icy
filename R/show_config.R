#' Show Environment Variable Status
#'
#' Displays the current values and status of environment variables, including
#' information about where each value comes from (e.g., .Renviron, local config,
#' or not set). This function provides comprehensive visibility into the
#' configuration state.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param var_names Optional character vector of specific variable names to show.
#'   If NULL (default), shows all variables defined in the configuration.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param display Character string specifying display mode:
#'   - "sources": Show variable names, values, and source information (default)
#'   - "values": Show only variable names and values (no source info)
#'   - "full": Show detailed information about all configuration sources
#'
#' @return Invisibly returns a data frame with variable names, values, and sources.
#'
#' @examples
#' \dontrun{
#' # Show all environment variables with sources
#' show_config("mypackage")
#'
#' # Show specific variables only
#' show_config("mypackage", var_names = c("API_KEY", "DB_HOST"))
#'
#' # Show only values without source information
#' show_config("mypackage", display = "values")
#'
#' # Show full detailed information
#' show_config("mypackage", display = "full")
#' }
#'
#' @export
show_config <- function(package = get_package_name(),
                        var_names = NULL,
                        user = "default",
                        display = "sources") {

  # Get all possible variable names if not specified
  if (is.null(var_names)) {
    # Try to get from template to see all possible variables
    template_vars <- tryCatch(
      {
        names(get_config(package = package,
                         origin = "template",
                         user = user))
      },
      error = function(e) NULL
    )

    # Also get from local config
    local_vars <- tryCatch(
      {
        names(get_config(package = package, origin = "local", user = user))
      },
      error = function(e) NULL
    )

    var_names <- unique(c(template_vars, local_vars))

    if (length(var_names) == 0) {
      .icy_warn(paste0("No variables found in configuration for ", package))
      return(invisible(NULL))
    }
  }

  # Build status information
  status_df <- data.frame(
    variable = var_names,
    value = character(length(var_names)),
    source = character(length(var_names)),
    stringsAsFactors = FALSE
  )

  # Get values from different sources
  renviron_config <- tryCatch(
    {
      get_config(package = package, origin = "renviron", user = user)
    },
    error = function(e) list()
  )

  local_config <- tryCatch(
    {
      get_config(package = package, origin = "local", user = user)
    },
    error = function(e) list()
  )

  # Check each variable
  for (i in seq_along(var_names)) {
    var <- var_names[i]

    # Check current session
    session_value <- Sys.getenv(var, unset = NA)

    # Determine value and source with priority: session > .Renviron > local config
    if (!is.na(session_value)) {
      status_df$value[i] <- session_value
      # Determine if session value matches a config source
      if (var %in% names(renviron_config) && !is.null(renviron_config[[var]]) && renviron_config[[var]] == session_value) {
        status_df$source[i] <- ".Renviron"
      } else if (var %in% names(local_config) && !is.null(local_config[[var]]) && local_config[[var]] == session_value) {
        status_df$source[i] <- "local config"
      } else {
        status_df$source[i] <- "session"
      }
    } else if (var %in% names(renviron_config) && !is.null(renviron_config[[var]])) {
      status_df$value[i] <- as.character(renviron_config[[var]])
      status_df$source[i] <- ".Renviron"
    } else if (var %in% names(local_config) && !is.null(local_config[[var]])) {
      status_df$value[i] <- as.character(local_config[[var]])
      status_df$source[i] <- "local config"
    } else {
      status_df$value[i] <- "(not set)"
      status_df$source[i] <- "not set"
    }
  }

  # Validate display mode
  valid_modes <- c("sources", "values", "full")
  if (!display %in% valid_modes) {
    .icy_stop(c(
      paste0("Invalid display mode: ", display),
      "i" = paste0("Valid modes are: ", paste(valid_modes, collapse = ", "))
    ))
  }

  # Display results based on mode
  if (display == "full") {
    .show_config_full(package, status_df, renviron_config, local_config, user)
  } else {
    .show_config_standard(package, status_df, display)
  }

  return(invisible(status_df))
}


#' Standard display mode for show_config
#' @keywords internal
.show_config_standard <- function(package, status_df, display) {
  .icy_title(paste0("Environment variables for ", package, ":"))

  for (i in seq_len(nrow(status_df))) {
    var <- status_df$variable[i]
    value <- status_df$value[i]
    source <- status_df$source[i]

    # Variable name without direct coloring
    colored_var <- var
    
    # Color-code the value using consistent helper function
    colored_value <- .format_value_with_color(var, value)

    # Format source information
    if (display == "sources" && value != "(not set)") {
      source_color <- switch(source,
        ".Renviron" = "red",
        "local config" = "blue", 
        "session" = "yellow",
        "not set" = "grey",
        "white"  # default
      )
      colored_source <- paste0(" [", source, "]")
      .icy_text(paste0(colored_var, " = ", colored_value, colored_source))
    } else {
      .icy_text(paste0(colored_var, " = ", colored_value))
    }
  }
}


#' Full display mode for show_config
#' @keywords internal
.show_config_full <- function(package, status_df, renviron_config, local_config, user) {
  .icy_title(paste0("Detailed configuration for ", package, ":"))
  
  # Show template configuration
  template_config <- tryCatch({
    get_config(package = package, origin = "template", user = user)
  }, error = function(e) list())
  
  if (length(template_config) > 0) {
    .icy_text("Template Configuration:")
    for (var in names(template_config)) {
      value <- template_config[[var]]
      colored_value <- .format_value_with_color(var, value, "(null)")
      .icy_text(paste0("  ", var, " = ", colored_value))
    }
    .icy_text("")
  }
  
  # Show local configuration
  if (length(local_config) > 0) {
    .icy_text("Local Configuration:")
    for (var in names(local_config)) {
      value <- local_config[[var]]
      colored_value <- .format_value_with_color(var, value, "(null)")
      .icy_text(paste0("  ", var, " = ", colored_value))
    }
    .icy_text("")
  }
  
  # Show .Renviron configuration
  if (length(renviron_config) > 0) {
    .icy_text(".Renviron Configuration:")
    for (var in names(renviron_config)) {
      value <- renviron_config[[var]]
      colored_value <- .format_value_with_color(var, value)
      .icy_text(paste0("  ", var, " = ", colored_value))
    }
    .icy_text("")
  }
  
  # Show session environment variables
  session_vars <- list()
  if (length(template_config) > 0) {
    for (var in names(template_config)) {
      session_value <- Sys.getenv(var, unset = NA)
      if (!is.na(session_value)) {
        session_vars[[var]] <- session_value
      }
    }
  }
  
  if (length(session_vars) > 0) {
    .icy_text("Session Environment:")
    for (var in names(session_vars)) {
      value <- session_vars[[var]]
      colored_value <- .format_value_with_color(var, value)
      .icy_text(paste0("  ", var, " = ", colored_value))
    }
    .icy_text("")
  }
  
  # Show final resolved values
  .icy_text("Final Resolved Values (Priority: Session > .Renviron > Local > Template):")
  for (i in seq_len(nrow(status_df))) {
    var <- status_df$variable[i]
    value <- status_df$value[i] 
    source <- status_df$source[i]
    
    # Use consistent value coloring
    colored_value <- .format_value_with_color(var, value)
    
    source_color <- switch(source,
      ".Renviron" = "red",
      "local config" = "blue",
      "session" = "yellow", 
      "not set" = "grey",
      "white"
    )
    
    .icy_text(paste0("  ", var, " = ", colored_value, 
                    " [", source, "]"))
  }
}


#' Format value with consistent color coding
#' @keywords internal
.format_value_with_color <- function(var, value, null_replacement = "(not set)") {
  if (is.null(value)) {
    return(null_replacement)
  }
  
  value_str <- as.character(value)
  
  # Return the value as-is without color formatting
  return(value_str)
}
