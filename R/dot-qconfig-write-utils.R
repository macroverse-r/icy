#' qconfig Configuration Writing Utilities
#'
#' Internal functions for writing configuration values and formatting messages
#' in the qconfig interactive configuration system.
#'
#' @name qconfig-write-utils
#' @keywords internal
NULL

#' Write Configuration Value
#'
#' Internal helper function to write configuration values to the specified location.
#'
#' @param var_name Variable name
#' @param value Value to write
#' @param write Write location ("local", "renviron", "session")
#' @param package Package name 
#' @param user User section
#' @param verbose Whether to show confirmation messages
#' @param type Expected type for the variable (NULL means no conversion)
#' @param fn_tmpl Optional path to custom YAML template file (NULL uses default template)
#' @param fn_local Optional path to custom local YAML file (NULL uses default local file)
#'
#' @return TRUE if successful, FALSE otherwise
#' @keywords internal
.write_config_value <- function(var_name, value, write, package, user, verbose, type = NULL, fn_tmpl = NULL, fn_local = NULL) {
  tryCatch({
    switch(write,
      "local" = {
        config_list <- list()
        # Convert value based on type information
        converted_value <- .convert_by_type(value, type)
        config_list[[var_name]] <- converted_value
        write_local(var_list = config_list, package = package, user = user, fn_tmpl = fn_tmpl, fn_local = fn_local)
        if (verbose) {
          .icy_success(paste0("Written ", var_name, " to local config"))
        }
        TRUE
      },
      "renviron" = {
        config_list <- list()
        # .Renviron always stores as strings, so use original value
        config_list[[var_name]] <- as.character(value)
        write_renviron(var_list = config_list)
        if (verbose) {
          .icy_success(paste0("Written ", var_name, " to ~/.Renviron"))
        }
        TRUE
      },
      "session" = {
        # Sys.setenv always stores as strings, so use original value
        do.call(Sys.setenv, stats::setNames(list(as.character(value)), var_name))
        if (verbose) {
          .icy_success(paste0("Set ", var_name, " in current session"))
        }
        TRUE
      }
    )
  }, error = function(e) {
    .icy_warn(paste0("Failed to write ", var_name, ": ", e$message))
    FALSE
  })
}

#' Format Success Message
#'
#' Internal helper function to format consistent success messages.
#'
#' @param var_name Variable name
#' @param value Selected or entered value
#' @param write Write location ("local", "renviron", "session")
#' @return Formatted success message
#' @keywords internal
.format_success_message <- function(var_name, value, write) {
  # Map write locations to display names
  location_display <- switch(write,
    "local" = "local config",
    "renviron" = ".Renviron", 
    "session" = "session",
    write  # fallback
  )
  
  paste0("Set ", var_name, " to ", value, " in ", location_display)
}