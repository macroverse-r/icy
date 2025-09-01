#' Messaging Functions for icy Package
#'
#' @description
#' Lightweight messaging functions using base R with ANSI color support.
#' No external dependencies - provides colored output with graceful fallback.
#' Enhanced versions available when contextual package is loaded.
#'
#' @name icy_messaging
NULL

#' Stop with formatted error message
#'
#' @description
#' Stops execution with a formatted error message. Uses red color
#' and error symbol when terminal supports it.
#'
#' @param msg Main error message
#' @param ... Additional arguments (currently unused, for compatibility)
.icy_stop <- function(msg, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    contextual::cx_stop(msg, .call_level = -2, ...)
  } else {
    stop(.apply_color(paste0("\u2717 ", msg), "red"), call. = FALSE)
  }
}

#' Warning with formatted message
#'
#' @description
#' Issues a warning with formatted message. Uses yellow color
#' and warning symbol when terminal supports it.
#'
#' @param msg Main warning message
#' @param ... Additional arguments (currently unused, for compatibility)
.icy_warn <- function(msg, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    contextual::cx_warn(msg, ...)
  } else {
    warning(.apply_color(paste0("! ", msg), "yellow"), call. = FALSE)
  }
}

#' Generic alert message
#'
#' @description
#' Generic alert function that uses contextual formatting when available.
#' Falls back to info alert when contextual is not available.
#'
#' @param msg Alert message
#' @param ... Additional arguments passed to underlying functions
.icy_alert <- function(msg, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    contextual::cx_alert(msg, ...)
  } else {
    message(.apply_color("! ", "yellow"), msg)
  }
}


#' Text output
#'
#' @description
#' Outputs text using contextual formatting when available.
#' Falls back to base R message() which respects suppressMessages().
#'
#' @param msg Text message
#' @param ... Additional arguments passed to underlying functions
.icy_text <- function(msg, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    contextual::cx_text(msg, ...)
  } else {
    message(msg)
  }
}

#' Success message
#'
#' @description
#' Displays a success message using contextual formatting when available.
#' Falls back to green checkmark with terminal color support.
#'
#' @param msg Success message
#' @param ... Additional arguments passed to underlying functions
.icy_success <- function(msg, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    contextual::cx_success(msg, ...)
  } else {
    message(.apply_color("\u2713 ", "green"), msg)
  }
}


#' Bulleted list output
#'
#' @description
#' Displays a bulleted list using contextual formatting when available.
#' Falls back to simple bullet points with basic formatting.
#'
#' @param items Named vector of items to display
#' @param ... Additional arguments passed to underlying functions
.icy_bullets <- function(items, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    contextual::cx_bullets(items, ...)
  } else {
    if (length(items) == 0) return(invisible())
    
    for (i in seq_along(items)) {
      name <- names(items)[i]
      value <- items[i]
      
      if (is.null(name) || name == "") {
        cat("\u2022 ", value, "\n", sep = "")
      } else {
        cat("\u2022 ", name, ": ", value, "\n", sep = "")
      }
    }
  }
}

#' Title heading
#'
#' @description
#' Displays a title heading using contextual formatting when available.
#' Falls back to bold formatting with dashes.
#'
#' @param msg Title text
#' @param level_adjust Numeric. Adjustment to title level for contextual. Default: 0.
#' @param ... Additional arguments passed to underlying functions
.icy_title <- function(msg, level_adjust = 0, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    # Use level_adjust = -1 to compensate for the call stack perspective difference
    # between .icy_title() and cx_title(). This ensures .icy_title() behaves 
    # exactly like cx_title() when called directly from the same context.
    contextual::cx_title(msg, level_adjust = level_adjust - 1, ...)
  } else {
    message(.apply_color(paste0("==== ", msg, " ===="),
                         color = "blue",
                         style = "bold"))
  }
}



#' Informational message
#'
#' @description
#' Informational message function that uses contextual formatting when available.
#' Falls back to simple message with info symbol when contextual is not available.
#'
#' @param msg Informational message
#' @param ... Additional arguments passed to underlying functions
.icy_inform <- function(msg, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    contextual::cx_inform(msg, ...)
  } else {
    message(.apply_color("\u2139 ", "blue"), msg)
  }
}
