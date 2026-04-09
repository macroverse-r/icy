#' Create Package Configuration Template
#'
#' Creates a new YAML configuration template file for a package through a
#' streamlined, interactive process. This function acts as a coordinator,
#' setting up the template structure and delegating content creation to
#' `update_template_interactive()`.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()`
#'   to detect the calling package.
#' @param name Optional character string for named configs (e.g., "gams_switches").
#'   If NULL (default), creates the main template file (\{package\}_template.yml).
#' @param overwrite Logical indicating whether to overwrite existing template file.
#'   If NULL (default), prompts user when file exists.
#' @param verbose Logical. If TRUE (default), displays progress and confirmation messages.
#' @param interactive Logical. If TRUE (default), enters interactive mode for
#'   variable configuration. If FALSE, creates an empty template structure.
#'
#' @return Character string with the full path to the created template file (invisible).
#'
#' @details
#' This function has been refactored to follow a coordinator pattern:
#' \itemize{
#'   \item Sets up the template file path and handles overwrite logic
#'   \item Creates the basic template structure with headers
#'   \item Delegates variable configuration to `update_template_interactive()`
#'   \item Provides helpful next steps for package developers
#' }
#'
#' The interactive mode guides you through adding variables with descriptions,
#' types, default values, and other metadata. The template follows icy's standard
#' structure for configuration management.
#'
#' @examples
#' \dontrun{
#' # Interactive template creation
#' create_template()
#'
#' # Create template for specific package
#' create_template(package = "mypackage")
#'
#' # Create a named template
#' create_template(name = "gams_switches")
#'
#' # Create empty template without interaction
#' create_template(interactive = FALSE)
#' }
#'
#' @seealso 
#' \code{\link{update_template}}, \code{\link{update_template_interactive}} for modifying existing templates
#' \code{\link{setup_config}} for interactive configuration of packages
#'
#' @export
create_template <- function(package = get_package_name(verbose = FALSE),
                           name = NULL,
                           overwrite = NULL,
                           verbose = TRUE,
                           interactive = TRUE) {

  # Step 1: Determine template file path
  template_path <- file.path("inst", .template_filename(package, name))
  
  # Step 2: Welcome message
  if (verbose) {
    .icy_title("Creating Configuration Template")
    .icy_text(paste0("Package: ", .apply_color(package, "cyan")))
    .icy_text(paste0("Template location: ", .apply_color(template_path, "yellow")))
  }
  
  # Step 3: Handle existing file
  if (file.exists(template_path)) {
    if (is.null(overwrite)) {
      .icy_text("")
      .icy_alert(paste0("Template file already exists: ", template_path))
      .icy_text(paste0("Overwrite existing file? ", .apply_color("(Y/n)", "gray")))
      response <- tolower(trimws(readline()))
      overwrite <- (response != "n" && response != "no")
    }
    
    if (!overwrite) {
      .icy_alert("Template creation cancelled")
      return(invisible(NULL))
    }
  }
  
  # Step 4: Ensure directory exists
  template_dir <- dirname(template_path)
  if (!dir.exists(template_dir)) {
    if (verbose) {
      .icy_text("")
      .icy_text(paste0("Create directory ", .apply_color(template_dir, "yellow"), "? ", .apply_color("(Y/n)", "gray")))
      response <- tolower(trimws(readline()))
      if (response == "n" || response == "no") {
        .icy_alert("Template creation cancelled")
        return(invisible(NULL))
      }
    }
    dir.create(template_dir, recursive = TRUE)
  }
  
  # Step 5: Initialize complete template structure
  # Initialize all sections to provide complete template structure from the start
  template_data <- list(
    default = list(),
    types = list(),
    descriptions = list(),
    notes = list(),
    options = list(),
    inheritances = list()
  )
  
  # Step 6: Create initial template file with header
  .write_template_with_header(
    template_data = template_data,
    file_path = template_path,
    package = package,
    verbose = verbose,
    auto_save = FALSE
  )
  
  # Step 7: Enter streamlined variable addition mode
  if (interactive) {
    result <- .streamlined_variable_collection(
      template_data = template_data,
      package = package,
      template_path = template_path,
      mode = "create",
      verbose = verbose
    )
    
    # Handle cancellation
    if (is.null(result)) {
      return(invisible(NULL))
    }
    
    template_data <- result
  }
  
  # Step 8: Show next steps
  if (verbose) {
    .show_next_steps(package, template_path)
  }
  
  return(invisible(template_path))
}



