#' Display Environment Variable Values
#'
#' Displays the current values of environment variables in the session.
#'
#' @param var_names Character vector of variable names to display.
#'   If NULL (default), uses all variables from the YAML config.
#'   
#' @return Invisibly returns a named list of the displayed variables and their values.
#' @export
display_env_vars <- function(var_names = NULL) {
  if (is.null(var_names)) {
    var_names <- get_env_var_names()
  }
  
  if (length(var_names) == 0) {
    return(invisible(list()))
  }
  
  # Create a list to store values
  values_list <- list()
  
  cli::cli_h3("Current environment variable values:")
  for (var in var_names) {
    value <- Sys.getenv(var, "(not set)")
    values_list[[var]] <- value
    
    # Format paths with .file instead of .val for consistency
    if (grepl("_DIR$", var) && value != "(not set)") {
      cli::cli_text("{.var {var}} = {.file {value}}")
    } else {
      cli::cli_text("{.var {var}} = {.val {value}}")
    }
  }
  
  invisible(values_list)
}
