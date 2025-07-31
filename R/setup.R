#' Interactive Package Setup
#'
#' Walks through all variables defined in the package template for interactive 
#' configuration. Provides a guided setup experience for first-time package users.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param write Character string specifying where to write the configuration.
#'   Options: "local" (default, writes to local YAML config), "renviron" (writes to ~/.Renviron),
#'   "session" (sets in current R session only using Sys.setenv).
#' @param skip_configured Logical. If TRUE (default), skips variables that already have
#'   non-default values configured. Set to FALSE to reconfigure all variables.
#' @param vars Character vector of variable names to configure. If NULL (default), 
#'   configures all variables from template. Also controls the order of configuration.
#' @param allow_skip Logical scalar or vector. Controls whether users can skip individual variables.
#'   If scalar, applies to all variables. If vector, must match length of variables to configure.
#' @param type Character scalar or vector. Specifies expected type for variables.
#'   If scalar, applies to all. If vector, must match length of variables. See `qconfig()` for valid types.
#' @param note Character scalar or vector. Additional notes to display for each variable.
#'   If scalar, same note for all. If vector, must match length of variables.
#' @param arg_only Logical scalar or vector. If TRUE, only uses provided options and ignores template options.
#'   If scalar, applies to all. If vector, must match length of variables.
#' @param verbose Logical. If TRUE, displays confirmation messages. Defaults to FALSE.
#'
#' @return A named list of all configured values (invisible). Values that were 
#'   skipped are included as NULL in the list.
#'
#' @details
#' This function reads all variables from the package template and presents them
#' for interactive configuration using qconfig(). It provides:
#' \itemize{
#'   \item Progress tracking through the configuration process
#'   \item Integration with template descriptions, options, and type detection
#'   \item Option to skip already-configured variables
#'   \item Flexible output destinations (local config, .Renviron, or session)
#' }
#'
#' @examples
#' \dontrun{
#' # Full package setup
#' setup()
#'
#' # Setup specific variables in custom order
#' setup(vars = c("DUMMY_API_KEY", "DUMMY_VERBOSE", "DUMMY_TIMEOUT"))
#'
#' # Reconfigure all variables including those already set
#' setup(skip_configured = FALSE)
#'
#' # Write to .Renviron instead of local config
#' setup(write = "renviron")
#'
#' # Don't allow skipping for critical variables
#' setup(vars = c("DUMMY_API_KEY", "DUMMY_DB_HOST", "DUMMY_VERBOSE"),
#'       allow_skip = c(FALSE, FALSE, TRUE))
#'
#' # Add custom notes for specific variables
#' setup(vars = c("DUMMY_API_KEY", "DUMMY_TIMEOUT"),
#'       note = c("Contact admin@example.com for API key", "Default is 30 seconds"))
#'
#' # Force specific types
#' setup(vars = c("DUMMY_PORT", "DUMMY_VERBOSE"),
#'       type = c("integer", "logical"))
#' }
#'
#' @export
setup <- function(package = get_package_name(), user = "default", write = "local",
                  skip_configured = FALSE, vars = NULL, allow_skip = TRUE,
                  type = NULL, note = NULL, arg_only = FALSE, verbose = FALSE) {
  
  # Get template variables
  template_config <- get_config(package = package, user = user, origin = "template")
  if (is.null(template_config) || length(template_config) == 0) {
    .icy_stop(paste0("No template configuration found for package '", package, "'"))
  }
  
  # Filter variables if specified
  var_names <- if (is.null(vars)) names(template_config) else intersect(vars, names(template_config))
  if (length(var_names) == 0) {
    .icy_inform("No variables to configure")
    return(invisible(NULL))
  }
  
  # Skip already configured variables if requested
  if (skip_configured) {
    current_config <- tryCatch(get_config(package = package, user = user, origin = "priority"), 
                              error = function(e) NULL)
    if (!is.null(current_config)) {
      configured <- character(0)
      for (var_name in var_names) {
        current_val <- current_config[[var_name]]
        template_val <- template_config[[var_name]]
        if (!is.null(current_val) && !identical(current_val, template_val)) {
          configured <- c(configured, var_name)
        }
      }
      if (length(configured) > 0) {
        .icy_inform(paste0("Skipping ", length(configured), " configured variable(s): ", 
                          paste(configured, collapse = ", ")))
        var_names <- setdiff(var_names, configured)
      }
    }
  }
  
  if (length(var_names) == 0) {
    .icy_success("All variables are already configured!")
    return(invisible(NULL))
  }
  
  # Prepare arguments for each variable
  # Convert scalar arguments to vectors of appropriate length
  n_vars <- length(var_names)
  allow_skip_vec <- if (length(allow_skip) == 1) rep(allow_skip, n_vars) else allow_skip
  type_vec <- if (is.null(type)) rep(NA_character_, n_vars) else if (length(type) == 1) rep(type, n_vars) else type
  note_vec <- if (is.null(note)) rep(NA_character_, n_vars) else if (length(note) == 1) rep(note, n_vars) else note
  arg_only_vec <- if (length(arg_only) == 1) rep(arg_only, n_vars) else arg_only
  
  # Validate vector lengths
  if (length(allow_skip_vec) != n_vars) {
    .icy_stop("allow_skip must be a scalar or vector of length matching variables to configure")
  }
  if (!is.null(type) && length(type_vec) != n_vars) {
    .icy_stop("type must be a scalar or vector of length matching variables to configure")
  }
  if (!is.null(note) && length(note_vec) != n_vars) {
    .icy_stop("note must be a scalar or vector of length matching variables to configure")
  }
  if (length(arg_only_vec) != n_vars) {
    .icy_stop("arg_only must be a scalar or vector of length matching variables to configure")
  }
  
  # Display intro
  .icy_title(paste0("Setup: ", package), auto_number = FALSE)
  .icy_text(paste0("Configuring ", length(var_names), " variable(s):"))
 
  # Display numbered list of variables to configure
  .icy_bullets(var_names, bullet = "1.")
  
  # Check if user wants to skip everything
  skip_info <- .apply_color("(or type 'skip'/'s' to skip the setup)", color = "gray")
  user_input <- readline(paste0("Press Enter to start configuration:\n", skip_info, "\n"))
  if (tolower(trimws(user_input)) %in% c("skip", "s")) {
    .icy_inform("Setup skipped")
    return(invisible(NULL))
  }
  
  # Configure each variable
  results <- list()
  for (i in seq_along(var_names)) {
    var_name <- var_names[i]
    
    # Progress indicator
    .icy_text("")
    .icy_title(paste0(var_name, " (", i, "/", length(var_names), ")"), auto_number = FALSE)
    
    # Configure variable with appropriate arguments
    result <- tryCatch({
      # Build qconfig arguments
      args <- list(
        var_name = var_name,
        package = package,
        user = user,
        write = write,
        allow_skip = allow_skip_vec[i],
        verbose = verbose,
        arg_only = arg_only_vec[i]
      )
      
      # Add optional arguments only if not NA
      if (!is.na(type_vec[i])) args$type <- type_vec[i]
      if (!is.na(note_vec[i])) args$note <- note_vec[i]
      
      # Call qconfig with prepared arguments
      do.call(qconfig, args)
    }, error = function(e) {
      .icy_warn(paste0("Failed to configure ", var_name, ": ", e$message))
      NULL
    })
    
    results[[var_name]] <- result
  }
  
  # Summary
  configured_count <- sum(!sapply(results, is.null))
  .icy_text("")
  .icy_title("Setup Complete", auto_number = FALSE)
  .icy_success(paste0("Configured ", configured_count, "/", length(var_names), " variables"))
  
  return(invisible(results))
}
