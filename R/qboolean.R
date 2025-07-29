#' Interactive Boolean Configuration Variable
#'
#' Prompts the user to configure a boolean environment variable using
#' TRUE/FALSE options, then writes the selected value to the specified 
#' configuration location. This function is a specialized wrapper around qconfig()
#' that integrates with icy's template system for boolean variables.
#'
#' @param var_name Character string with the environment variable name (e.g., "DUMMY_VERBOSE").
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param description Character string with custom description. If NULL, uses description
#'   from template YAML descriptions section. If no template description exists, no description is shown.
#' @param default Logical value indicating the default answer. TRUE for yes (default), FALSE for no.
#' @param allow_skip Logical. If TRUE, allows user to skip configuration by pressing Enter
#'   without selecting an option. Returns NULL when skipped. Defaults to TRUE.
#' @param note Character string with additional informational note to display before options.
#' @param write Character string specifying where to write the configuration.
#'   Options: "local" (default, writes to local YAML config), "renviron" (writes to ~/.Renviron),
#'   "session" (sets in current R session only using Sys.setenv).
#' @param verbose Logical. If TRUE, displays confirmation messages. Defaults to FALSE.
#'
#' @return Logical value (TRUE/FALSE), or NULL if allow_skip = TRUE and user skips.
#'   The value is written to the specified configuration location before being returned.
#'
#' @details
#' This function is a specialized version of qconfig() designed specifically for
#' boolean configuration variables. It:
#' \itemize{
#'   \item Uses TRUE/FALSE as fixed options (ignores template options section)
#'   \item Integrates with template descriptions section automatically
#'   \item Returns logical values instead of character strings
#'   \item Provides a simpler interface for boolean configuration
#' }
#'
#' The function displays "TRUE" and "FALSE" as options, with the default option
#' indicated. Users can select by number, and the result is converted to logical.
#'
#' @examples
#' \dontrun{
#' # Basic usage with template integration (writes to local config)
#' verbose_mode <- qboolean("DUMMY_VERBOSE", package = "dummy")
#' # Uses template description, presents TRUE/FALSE options, writes to local config
#'
#' # Write to ~/.Renviron instead
#' debug_mode <- qboolean("DUMMY_DEBUG", write = "renviron", default = FALSE)
#' # Writes boolean value to ~/.Renviron file
#'
#' # Set only in current session
#' optional_flag <- qboolean("DUMMY_OPTIONAL", write = "session")
#' # Sets boolean value using Sys.setenv() for current session only
#'
#' # Skip functionality - no writing occurs
#' skipped_setting <- qboolean("DUMMY_SKIPPED", allow_skip = TRUE)
#' # Can return NULL if user skips, no configuration is written
#'
#' # Configuration is now automatic - no manual write_local needed
#' verbose_setting <- qboolean("DUMMY_VERBOSE", package = "dummy")
#' # Value is automatically written to config during the function call
#' }
#'
#' @export
qboolean <- function(var_name, package = get_package_name(), user = "default",
                     description = NULL, default = TRUE, allow_skip = TRUE, 
                     note = NULL, write = "local", verbose = FALSE) {
  # Prepare boolean options with default first
  options <- if (default) c("TRUE", "FALSE") else c("FALSE", "TRUE")
  
  # Call qconfig with logical type (automatic boolean behavior)
  return(qconfig(
    var_name = var_name, package = package, user = user, description = description,
    options = options, allow_skip = allow_skip, note = note, 
    arg_only = TRUE, write = write, type = "logical", verbose = verbose
  ))
}