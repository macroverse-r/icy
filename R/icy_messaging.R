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
.icy_abort <- function(msg, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    # Get the calling function (skip .icy_abort wrapper)
    call_info <- sys.call(-1)
    if (!is.null(call_info)) {
      func_name <- as.character(call_info[[1]])
      # Format message with function name and message
      formatted_message <- paste0("in ", 
                                  cli::style_bold(cli::col_blue(func_name)), 
                                  "(): ", 
                                  cli::col_red(msg))
    } else {
      formatted_message <- cli::col_red(msg)
    }
    cli::cli_abort(c("x" = formatted_message), call = NULL, ...)
  } else {
    stop(.apply_color(paste0("✗ ", msg), "red"), call. = FALSE)
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

#' Information message
#'
#' @description
#' Displays an information message using base R message().
#' Plain text output, no special formatting.
#'
#' @param msg Information message
#' @param ... Additional arguments (currently unused, for compatibility)
.icy_inform <- function(msg, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    contextual::cx_text(msg, ...)
  } else {
    message(msg)
  }
}

#' Plain text output
#'
#' @description
#' Outputs plain text using cat(). No special formatting or colors.
#'
#' @param msg Text to output
#' @param ... Additional arguments passed to cat()
.icy_text <- function(msg, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    contextual::cx_text(msg, ...)
  } else {
    cat(msg, "\n", sep = "")
  }
}

#' Success alert message
#'
#' @description
#' Displays a success message with green checkmark when terminal supports it.
#'
#' @param msg Success message
.icy_alert_success <- function(msg) {
  message(.apply_color("✓ ", "green"), msg)
}

#' Warning alert message
#'
#' @description
#' Displays a warning message with yellow exclamation mark when terminal supports it.
#'
#' @param msg Warning message
.icy_alert_warning <- function(msg) {
  message(.apply_color("! ", "yellow"), msg)
}

#' Information alert message
#'
#' @description
#' Displays an information message with blue info symbol when terminal supports it.
#'
#' @param msg Information message
.icy_alert_info <- function(msg) {
  message(.apply_color("i ", "blue"), msg)
}

#' Danger alert message
#'
#' @description
#' Displays a danger/error message with red X mark when terminal supports it.
#'
#' @param msg Danger message
.icy_alert_danger <- function(msg) {
  message(.apply_color("✗ ", "red"), msg)
}

#' Bulleted list output
#'
#' @description
#' Simple implementation of bulleted list. Takes a named vector
#' and outputs with basic bullet points.
#'
#' @param items Named vector of items to display
.icy_bullets <- function(items) {
  if (length(items) == 0) return(invisible())
  
  for (i in seq_along(items)) {
    name <- names(items)[i]
    value <- items[i]
    
    if (is.null(name) || name == "") {
      cat("• ", value, "\n", sep = "")
    } else {
      cat("• ", name, ": ", value, "\n", sep = "")
    }
  }
}

#' Heading 3 style output
#'
#' @description
#' Simple heading output with basic formatting.
#'
#' @param msg Heading text
.icy_h3 <- function(msg) {
  message(.apply_color(paste0("--- ", msg, " ---"), style = "bold"))
}

#' Enhanced information message (contextual integration)
#'
#' @description
#' Enhanced version of icy_inform that uses contextual package
#' for rich formatting when available, falls back to basic message otherwise.
#'
#' @param msg Information message
#' @param ... Additional message components
.icy_enhanced_inform <- function(msg, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    contextual::cx_text(msg, ...)
  } else {
    .icy_inform(msg, ...)
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
    .icy_alert_info(msg)
  }
}

#' Debug message
#'
#' @description
#' Debug message function that uses contextual formatting when available.
#' Falls back to simple message output when contextual is not available.
#'
#' @param msg Debug message
#' @param ... Additional arguments passed to underlying functions
.icy_debug <- function(msg, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    contextual::cx_debug(msg, ...)
  } else {
    message("[DEBUG] ", msg)
  }
}

#' Enhanced alert message (contextual integration)
#'
#' @description
#' Enhanced version of alert messages that uses contextual package
#' for rich formatting when available.
#'
#' @param msg Alert message
#' @param type Type of alert ("info", "warning", "success", "danger")
.icy_enhanced_alert <- function(msg, type = "info") {
  if (requireNamespace("contextual", quietly = TRUE)) {
    switch(type,
      "info" = contextual::cx_alert(msg),
      "warning" = contextual::cx_warn(msg),
      "success" = contextual::cx_success(msg),
      "danger" = contextual::cx_stop(msg),
      contextual::cx_alert(msg)
    )
  } else {
    switch(type,
      "info" = .icy_alert_info(msg),
      "warning" = .icy_alert_warning(msg),
      "success" = .icy_alert_success(msg),
      "danger" = .icy_alert_danger(msg),
      .icy_alert_info(msg)
    )
  }
}