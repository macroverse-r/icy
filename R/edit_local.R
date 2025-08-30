#' Edit Local Configuration File
#'
#' Opens the local YAML configuration file for editing in your preferred editor.
#' This provides a better experience than just \code{file.edit(find_local())} by
#' offering editor selection, validation, and session synchronization options.
#'
#' The function locates the local configuration file using the existing fuzzy
#' matching system, opens it in the specified editor, and optionally validates
#' YAML syntax and syncs changes to the current R session after editing.
#'
#' @param package Character string with the package name. Defaults to \code{get_package_name()} to detect the calling package.
#' @param fn_local Character string with custom filename for the local config.
#'   If NULL, uses the fuzzy finder to locate the standard local config file.
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
#' @param sync Logical. If TRUE, automatically syncs changes to current R session
#'   using \code{sync()} after editing. Defaults to FALSE.
#' @param verbose Logical. If TRUE (default), displays informative messages about
#'   the editing process.
#'
#' @return Invisibly returns the path to the edited file.
#'
#' @examples
#' \dontrun{
#' # Open local config in default editor
#' edit_local("mypackage")
#'
#' # Open in VS Code and sync changes to session
#' edit_local("mypackage", editor = "vscode", sync = TRUE)
#'
#' # Edit specific config file without validation
#' edit_local("mypackage", fn_local = "custom_config.yml", validate = FALSE)
#'
#' # Quiet editing (no messages)
#' edit_local("mypackage", verbose = FALSE)
#' }
#'
#' @seealso \code{\link{find_local}} for locating config files,
#'   \code{\link{create_local}} for creating new config files,
#'   \code{\link{sync}} for syncing changes to R session
#'
#' @export
edit_local <- function(package = get_package_name(),
                      fn_local = NULL,
                      editor = "auto",
                      validate = TRUE,
                      sync = FALSE,
                      verbose = TRUE) {
  
  # Find the local config file
  local_path <- find_config_files(
    package = package,
    fn_local = fn_local,
    verbose = FALSE
  )$fn_local
  
  # Error if file not found
  if (is.null(local_path)) {
    .icy_stop(c(
      "No local configuration file found.",
      "i" = paste0("Run create_local(\"", package, "\") to create one first.")
    ))
  }
  
  if (verbose) {
    .icy_text(paste0("Opening local config: ", local_path))
  }
  
  # Open in specified editor
  .open_in_editor(local_path, editor, verbose)
  
  # Validate YAML syntax after editing
  if (validate) {
    .validate_yaml_syntax(local_path, verbose)
  }
  
  # Sync changes to R session if requested
  if (sync) {
    if (verbose) {
      .icy_text("Syncing changes to R session...")
    }
    sync(package = package, verbose = verbose)
  }
  
  return(invisible(local_path))
}

#' Open File in Specified Editor
#'
#' Internal function to handle editor selection and file opening.
#'
#' @param file_path Path to file to edit
#' @param editor Editor specification
#' @param verbose Whether to show messages
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
    if (.is_rstudio_available()) {
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

#' Check if RStudio API is Available
#'
#' @return Logical indicating if RStudio is available
#' @keywords internal
.is_rstudio_available <- function() {
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
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