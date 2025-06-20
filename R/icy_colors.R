#' ANSI Color Support for icy Package
#'
#' @description
#' Lightweight ANSI color functions with terminal detection.
#' No external dependencies - uses direct ANSI escape codes.
#'
#' @name icy_colors
NULL

#' Check if terminal supports colors
#'
#' @description
#' Detects if the current terminal supports ANSI color codes.
#' Respects NO_COLOR environment variable and checks for TTY.
#'
#' @return Logical indicating if colors are supported
#' @keywords internal
has_color <- function() {
  # Respect NO_COLOR environment variable
  if (Sys.getenv("NO_COLOR", "") != "") return(FALSE)
  
  # Check if output is going to a terminal
  if (!isatty(stdout())) return(FALSE)
  
  # Windows-specific checks
  if (Sys.info()[["sysname"]] == "Windows") {
    # Check for ConEmu or ANSICON
    return(Sys.getenv("ConEmuANSI", "") == "ON" || 
           Sys.getenv("ANSICON", "") != "")
  }
  
  # Unix-like systems generally support ANSI
  TRUE
}

#' Raw ANSI color functions
#'
#' @description
#' Direct ANSI escape code functions. These always apply color codes
#' regardless of terminal support. Use the safe `col_*` functions instead.
#'
#' @param x Text to colorize
#' @return Text with ANSI color codes
#' @keywords internal
#' @name ansi_colors
NULL

#' @rdname ansi_colors
ansi_red <- function(x) paste0("\033[31m", x, "\033[39m")

#' @rdname ansi_colors
ansi_green <- function(x) paste0("\033[32m", x, "\033[39m")

#' @rdname ansi_colors
ansi_yellow <- function(x) paste0("\033[33m", x, "\033[39m")

#' @rdname ansi_colors
ansi_blue <- function(x) paste0("\033[34m", x, "\033[39m")

#' @rdname ansi_colors
ansi_magenta <- function(x) paste0("\033[35m", x, "\033[39m")

#' @rdname ansi_colors
ansi_cyan <- function(x) paste0("\033[36m", x, "\033[39m")

#' @rdname ansi_colors
ansi_bold <- function(x) paste0("\033[1m", x, "\033[22m")

#' Safe color functions with terminal detection
#'
#' @description
#' Color functions that automatically detect terminal support and only
#' apply colors when appropriate. Safe to use in all contexts.
#'
#' @param x Text to colorize
#' @return Colored text if terminal supports it, plain text otherwise
#' @name safe_colors
NULL

#' @rdname safe_colors
col_red <- function(x) if (has_color()) ansi_red(x) else x

#' @rdname safe_colors
col_green <- function(x) if (has_color()) ansi_green(x) else x

#' @rdname safe_colors
col_yellow <- function(x) if (has_color()) ansi_yellow(x) else x

#' @rdname safe_colors
col_blue <- function(x) if (has_color()) ansi_blue(x) else x

#' @rdname safe_colors
col_magenta <- function(x) if (has_color()) ansi_magenta(x) else x

#' @rdname safe_colors
col_cyan <- function(x) if (has_color()) ansi_cyan(x) else x

#' @rdname safe_colors
col_bold <- function(x) if (has_color()) ansi_bold(x) else x