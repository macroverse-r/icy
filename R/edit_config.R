#' Edit Configuration File
#'
#' Opens the YAML configuration file for editing in your preferred editor.
#' This provides a better experience than just manually opening the file by
#' offering editor selection and YAML validation.
#'
#' @param package Character string with the package name. Defaults to \code{get_package_name()} to detect the calling package.
#' @param name Optional character string for named configs (e.g., "gams_switches").
#'   If NULL (default), opens the main config file (\{package\}_config.yml).
#' @param editor Character string specifying which editor to use:
#'   \itemize{
#'     \item "auto" (default): Use R's default \code{file.edit()}
#'     \item "rstudio": Force RStudio editor if available
#'     \item "vscode" or "code": Open in Visual Studio Code
#'     \item "vim", "nano", "emacs": Terminal editors
#'   }
#'   Falls back to \code{file.edit()} if specified editor is unavailable.
#' @param validate Logical. If TRUE (default), validates YAML syntax after editing
#'   and shows warnings for syntax errors. Does not prevent saving.
#' @param verbose Logical. If TRUE (default), displays informative messages about
#'   the editing process.
#'
#' @return Invisibly returns the path to the edited file.
#'
#' @examples
#' \dontrun{
#' # Open config in default editor
#' edit_config("mypackage")
#'
#' # Open in VS Code
#' edit_config("mypackage", editor = "vscode")
#'
#' # Edit a named config
#' edit_config("mypackage", name = "gams_switches")
#' }
#'
#' @seealso \code{\link{create_config}} for creating new config files,
#'   \code{\link{get_config}} for reading config values
#'
#' @export
edit_config <- function(package = get_package_name(),
                        name = NULL,
                        editor = "auto",
                        validate = TRUE,
                        verbose = TRUE) {

  # Find the config file
  config_path <- .find_config_files(
    package = package,
    name = name,
    verbose = FALSE
  )$fn_config

  # Error if file not found
  if (is.null(config_path)) {
    .icy_stop(c(
      "No configuration file found.",
      "i" = paste0("Run create_config(\"", package, "\") to create one first.")
    ))
  }

  if (verbose) {
    .icy_text(paste0("Opening config: ", config_path))
  }

  # Open in specified editor
  .open_in_editor(config_path, editor, verbose)

  # Validate YAML syntax after editing
  if (validate) {
    .validate_yaml_syntax(config_path, verbose)
  }

  return(invisible(config_path))
}

#' Open File in Specified Editor
#'
#' Internal function to handle editor selection and file opening.
#'
#' @param file_path Path to file to edit
#' @param editor Editor specification
#' @param verbose Whether to show messages
#' @importFrom utils file.edit
#' @keywords internal
.open_in_editor <- function(file_path, editor, verbose) {
  
  # Handle editor selection
  if (editor == "auto" || editor == "default") {
    if (verbose) {
      .icy_text("Using R's default editor")
    }
    file.edit(file_path)
    return()
  }
  
  # Try specific editors
  success <- FALSE
  
  if (editor %in% c("rstudio", "rs")) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      if (verbose) {
        .icy_text("Opening in RStudio")
      }
      rstudioapi::navigateToFile(file_path)
      success <- TRUE
    }
  } else if (editor %in% c("vscode", "code", "vs")) {
    if (.is_command_available("code")) {
      if (verbose) {
        .icy_text("Opening in Visual Studio Code")
      }
      system(paste0("code \"", file_path, "\""), wait = FALSE)
      success <- TRUE
    }
  } else if (editor %in% c("vim", "vi")) {
    if (.is_command_available("vim")) {
      if (verbose) {
        .icy_text("Opening in vim")
      }
      system(paste0("vim \"", file_path, "\""))
      success <- TRUE
    }
  } else if (editor == "nano") {
    if (.is_command_available("nano")) {
      if (verbose) {
        .icy_text("Opening in nano")
      }
      system(paste0("nano \"", file_path, "\""))
      success <- TRUE
    }
  } else if (editor == "emacs") {
    if (.is_command_available("emacs")) {
      if (verbose) {
        .icy_text("Opening in emacs")
      }
      system(paste0("emacs \"", file_path, "\""))
      success <- TRUE
    }
  }
  
  # Fall back to file.edit if specified editor failed
  if (!success) {
    if (verbose && editor != "auto") {
      .icy_warn(paste0("Editor '", editor, "' not available. Using R's default editor."))
    }
    file.edit(file_path)
  }
}

#' Check if System Command is Available
#'
#' @param command Command name to check
#' @return Logical indicating if command exists
#' @keywords internal
.is_command_available <- function(command) {
  suppressWarnings(system(paste0("which ", command), 
                         ignore.stdout = TRUE, 
                         ignore.stderr = TRUE) == 0)
}

#' Validate YAML Syntax
#'
#' Internal function to validate YAML syntax after editing.
#'
#' @param file_path Path to YAML file to validate
#' @param verbose Whether to show messages
#' @keywords internal
.validate_yaml_syntax <- function(file_path, verbose) {
  
  validation_result <- tryCatch(
    {
      yaml::read_yaml(file_path)
      "valid"
    },
    error = function(e) {
      list(error = e$message)
    }
  )
  
  if (identical(validation_result, "valid")) {
    if (verbose) {
      .icy_success("YAML syntax is valid")
    }
  } else {
    .icy_warn(c(
      "YAML syntax error detected:",
      "x" = validation_result$error,
      "i" = "Please check your YAML formatting"
    ))
  }
}