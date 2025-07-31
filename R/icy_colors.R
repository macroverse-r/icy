#' Internal ANSI Color Support
#'
#' @description
#' Internal color functions for icy package messaging.
#' Not exported - contextual handles user-facing color functionality.
#'
#' @keywords internal
#' @name icy_colors_internal
NULL

#' Check if terminal supports colors
#'
#' @return Logical indicating if colors are supported
#' @keywords internal
.has_color <- function() {
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

#' Apply ANSI color to text
#'
#' @param x Text to colorize
#' @param color Color name or ANSI code
#' @param style Optional style (bold, italic, etc.)
#' @return Colored text if terminal supports it, plain text otherwise
#' @keywords internal
.apply_color <- function(x, color = NULL, style = NULL) {
  if (!.has_color()) return(x)
  
  # ANSI color codes
  colors <- list(
    red = 31,
    green = 32,
    yellow = 33,
    blue = 34,
    magenta = 35,
    cyan = 36,
    white = 37,
    grey = 90,
    gray = 90,
    # 256-color mode colors
    orange = "38;5;208",  # True orange
    yellow256 = "38;5;220",  # Better yellow
    brown = "38;5;130"   # Brown for strings
  )
  
  # ANSI style codes
  styles <- list(
    bold = 1,
    italic = 3,
    underline = 4
  )
  
  result <- x
  
  # Apply color
  if (!is.null(color) && color %in% names(colors)) {
    color_code <- colors[[color]]
    if (is.numeric(color_code)) {
      # Standard 8/16 color mode
      result <- paste0("\033[", color_code, "m", result, "\033[39m")
    } else {
      # 256-color mode
      result <- paste0("\033[", color_code, "m", result, "\033[39m")
    }
  }
  
  # Apply style
  if (!is.null(style) && style %in% names(styles)) {
    style_code <- styles[[style]]
    # Reset codes: bold->22, italic->23, underline->24
    reset_code <- switch(style,
      bold = 22,
      italic = 23,
      underline = 24
    )
    result <- paste0("\033[", style_code, "m", result, "\033[", reset_code, "m")
  }
  
  result
}

