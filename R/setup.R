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
#'   configures all variables from template.
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
#' # Setup specific variables only
#' setup(vars = c("DUMMY_API_KEY", "DUMMY_VERBOSE"))
#'
#' # Reconfigure all variables including those already set
#' setup(skip_configured = FALSE)
#'
#' # Write to .Renviron instead of local config
#' setup(write = "renviron")
#' }
#'
#' @export
setup <- function(package = get_package_name(), user = "default", write = "local",
                  skip_configured = FALSE, vars = NULL, verbose = FALSE) {
  
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
  
  # Display intro
  .icy_title(paste0("Setup: ", package), auto_number = FALSE)
  .icy_text(paste0("Configuring ", length(var_names), " variable(s). Type 'skip' to skip the entire setup."))
  .icy_text("")
  
  # Check if user wants to skip everything
  user_input <- readline("Press Enter to continue or type 'skip': ")
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
    
    # Configure variable
    result <- tryCatch({
      qconfig(var_name = var_name, package = package, user = user, 
              write = write, allow_skip = TRUE, verbose = verbose)
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
