#' Package Configuration Wizard
#'
#' Comprehensive configuration wizard that walks through all variables defined
#' in the package template. Provides an interactive setup experience for users
#' to configure their entire package environment.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param write Character string specifying where to write the configuration.
#'   Options: "local" (default, writes to local YAML config), "renviron" (writes to ~/.Renviron),
#'   "session" (sets in current R session only using Sys.setenv).
#' @param var_filter Character vector of variable names to include. If NULL (default), 
#'   includes all variables from template. Use this to configure only specific variables.
#' @param skip_configured Logical. If TRUE (default), skips variables that already have
#'   non-default values configured. Set to FALSE to reconfigure all variables.
#' @param show_progress Logical. If TRUE (default), shows progress indicator during configuration.
#' @param allow_skip_all Logical. If TRUE (default), allows users to skip the entire wizard.
#' @param verbose Logical. If TRUE, displays confirmation messages. Defaults to FALSE.
#'
#' @return A named list of all configured values, or NULL if the wizard is skipped entirely.
#'   Values that were skipped individually are included as NULL in the list.
#'
#' @details
#' This function provides a comprehensive configuration experience by:
#' \itemize{
#'   \item Reading all variables from the package template
#'   \item Presenting them in a logical order with full context
#'   \item Showing current values and allowing reconfiguration
#'   \item Providing progress tracking for large configuration sets
#'   \item Supporting filtering to configure only specific variables
#'   \item Integrating all qconfig features (descriptions, options, type detection)
#' }
#'
#' The wizard will display:
#' \itemize{
#'   \item Overall progress (e.g., "Configuring dummy package (3/8)")
#'   \item Variable-specific titles and context
#'   \item Current configuration status
#'   \item Template descriptions and options
#'   \item Skip and navigation options
#' }
#'
#' @examples
#' \dontrun{
#' # Full package configuration wizard
#' config_wizard()
#'
#' # Configure specific variables only
#' config_wizard(var_filter = c("DUMMY_API_KEY", "DUMMY_VERBOSE"))
#'
#' # Reconfigure everything, including already-set variables
#' config_wizard(skip_configured = FALSE)
#'
#' # Write to .Renviron instead of local config
#' config_wizard(write = "renviron")
#'
#' # Silent mode with no progress indicators
#' config_wizard(show_progress = FALSE, verbose = FALSE)
#' }
#'
#' @export
config_wizard <- function(package = get_package_name(), user = "default", write = "local",
                          var_filter = NULL, skip_configured = TRUE, show_progress = TRUE,
                          allow_skip_all = TRUE, verbose = FALSE) {
  
  # Validate parameters
  params <- .validate_wizard_params(package, user, write, var_filter, skip_configured, 
                                   show_progress, allow_skip_all, verbose)
  
  # Get all template variables
  template_config <- tryCatch({
    get_config(package = params$package, user = params$user, origin = "template")
  }, error = function(e) {
    .icy_stop(paste0("Could not read template configuration for package '", params$package, "': ", e$message))
  })
  
  if (is.null(template_config) || length(template_config) == 0) {
    .icy_stop(paste0("No template configuration found for package '", params$package, "'"))
  }
  
  # Filter variables if requested
  var_names <- names(template_config)
  if (!is.null(params$var_filter)) {
    missing_vars <- setdiff(params$var_filter, var_names)
    if (length(missing_vars) > 0) {
      .icy_warn(paste0("Variables not found in template: ", paste(missing_vars, collapse = ", ")))
    }
    var_names <- intersect(params$var_filter, var_names)
  }
  
  if (length(var_names) == 0) {
    .icy_inform("No variables to configure")
    return(invisible(NULL))
  }
  
  # Filter out already configured variables if requested
  if (params$skip_configured) {
    current_config <- tryCatch({
      get_config(package = params$package, user = params$user, origin = "priority")
    }, error = function(e) NULL)
    
    if (!is.null(current_config)) {
      configured_vars <- .find_configured_variables(var_names, template_config, current_config)
      if (length(configured_vars) > 0) {
        .icy_inform(paste0("Skipping ", length(configured_vars), " already configured variable(s): ", 
                          paste(configured_vars, collapse = ", ")))
        var_names <- setdiff(var_names, configured_vars)
      }
    }
  }
  
  if (length(var_names) == 0) {
    .icy_success("All variables are already configured!")
    return(invisible(NULL))
  }
  
  # Main wizard introduction
  .display_wizard_intro(params$package, var_names, params$show_progress, params$allow_skip_all)
  
  # Allow skipping entire wizard
  if (params$allow_skip_all) {
    if (.prompt_skip_wizard()) {
      .icy_inform("Configuration wizard skipped")
      return(invisible(NULL))
    }
  }
  
  # Main configuration loop
  results <- list()
  
  for (i in seq_along(var_names)) {
    var_name <- var_names[i]
    
    # Display progress and variable context
    if (params$show_progress) {
      .display_variable_progress(params$package, var_name, i, length(var_names))
    }
    
    # Configure this variable
    tryCatch({
      result <- qconfig(
        var_name = var_name,
        package = params$package,
        user = params$user,
        write = params$write,
        allow_skip = TRUE,
        verbose = params$verbose
      )
      results[[var_name]] <- result
    }, error = function(e) {
      .icy_warn(paste0("Failed to configure ", var_name, ": ", e$message))
      results[[var_name]] <- NULL
    })
    
    # Add spacing between variables (except for last one)
    if (i < length(var_names)) {
      .icy_text("")
    }
  }
  
  # Final summary
  .display_wizard_summary(results, params$package, params$write)
  
  return(invisible(results))
}

#' @keywords internal
.validate_wizard_params <- function(package, user, write, var_filter, skip_configured, 
                                   show_progress, allow_skip_all, verbose) {
  
  if (!is.character(package) || length(package) != 1 || nchar(package) == 0) {
    .icy_stop("package must be a non-empty character string")
  }
  
  if (!is.character(user) || length(user) != 1 || nchar(user) == 0) {
    .icy_stop("user must be a non-empty character string")
  }
  
  if (!is.character(write) || length(write) != 1 || !write %in% c("local", "renviron", "session")) {
    .icy_stop("write must be one of: 'local', 'renviron', 'session'")
  }
  
  if (!is.null(var_filter) && (!is.character(var_filter) || length(var_filter) == 0)) {
    .icy_stop("var_filter must be NULL or a non-empty character vector")
  }
  
  if (!is.logical(skip_configured) || length(skip_configured) != 1 || is.na(skip_configured)) {
    .icy_stop("skip_configured must be TRUE or FALSE")
  }
  
  if (!is.logical(show_progress) || length(show_progress) != 1 || is.na(show_progress)) {
    .icy_stop("show_progress must be TRUE or FALSE")
  }
  
  if (!is.logical(allow_skip_all) || length(allow_skip_all) != 1 || is.na(allow_skip_all)) {
    .icy_stop("allow_skip_all must be TRUE or FALSE")
  }
  
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    .icy_stop("verbose must be TRUE or FALSE")
  }
  
  return(list(
    package = package,
    user = user,
    write = write,
    var_filter = var_filter,
    skip_configured = skip_configured,
    show_progress = show_progress,
    allow_skip_all = allow_skip_all,
    verbose = verbose
  ))
}

#' @keywords internal
.find_configured_variables <- function(var_names, template_config, current_config) {
  configured <- character(0)
  
  for (var_name in var_names) {
    template_value <- template_config[[var_name]]
    current_value <- current_config[[var_name]]
    
    # Consider configured if:
    # 1. Current value exists and is not NULL
    # 2. Current value differs from template default
    # 3. Template value is NULL/empty but current has a value
    if (!is.null(current_value) && 
        (!identical(current_value, template_value) || is.null(template_value) || template_value == "")) {
      configured <- c(configured, var_name)
    }
  }
  
  return(configured)
}

#' @keywords internal
.display_wizard_intro <- function(package, var_names, show_progress, allow_skip_all) {
  .icy_title(paste0("Configuration Wizard: ", package), auto_number = FALSE)
  
  count_msg <- if (length(var_names) == 1) {
    "1 variable to configure"
  } else {
    paste0(length(var_names), " variables to configure")
  }
  
  .icy_text(paste0("This wizard will help you configure ", count_msg, ":"))
  .icy_text("")
  .icy_bullets(var_names, bullet = "•")
  .icy_text("")
  
  if (allow_skip_all) {
    .icy_text("Press Enter to start configuration, or type 'skip' to exit the wizard.")
  } else {
    .icy_text("Press Enter to start configuration.")
  }
}

#' @keywords internal
.prompt_skip_wizard <- function() {
  user_input <- readline()
  return(tolower(trimws(user_input)) %in% c("skip", "s", "exit", "quit"))
}

#' @keywords internal
.display_variable_progress <- function(package, var_name, current, total) {
  progress_text <- paste0("Configuring ", package, " (", current, "/", total, ")")
  .icy_title(progress_text, auto_number = FALSE)
  .icy_title(var_name, auto_number = FALSE)
}

#' @keywords internal
.display_wizard_summary <- function(results, package, write) {
  .icy_text("")
  .icy_title("Configuration Complete", auto_number = FALSE)
  
  configured_count <- sum(!sapply(results, is.null))
  skipped_count <- sum(sapply(results, is.null))
  total_count <- length(results)
  
  if (configured_count > 0) {
    .icy_success(paste0("Configured ", configured_count, " variable(s) for ", package))
  }
  
  if (skipped_count > 0) {
    .icy_inform(paste0("Skipped ", skipped_count, " variable(s)"))
  }
  
  # Show where configurations were written
  write_location <- switch(write,
    "local" = "local configuration file",
    "renviron" = "~/.Renviron file", 
    "session" = "current R session",
    write
  )
  
  if (configured_count > 0) {
    .icy_text(paste0("Settings written to: ", write_location))
    .icy_text("")
    .icy_text("You can:")
    .icy_bullets(c(
      "View settings: show_config(package = \"" %paste0% package %paste0% "\")",
      "Modify settings: qconfig(\"VARIABLE_NAME\", package = \"" %paste0% package %paste0% "\")",
      "Run wizard again: config_wizard(package = \"" %paste0% package %paste0% "\")"
    ), bullet = "•")
  }
}

# Helper operator for string concatenation
`%paste0%` <- function(x, y) paste0(x, y)