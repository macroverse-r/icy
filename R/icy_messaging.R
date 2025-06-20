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
#' @export
icy_abort <- function(msg, ...) {
  stop(col_red(paste0("✗ ", msg)), call. = FALSE)
}

#' Warning with formatted message
#'
#' @description
#' Issues a warning with formatted message. Uses yellow color
#' and warning symbol when terminal supports it.
#'
#' @param msg Main warning message
#' @param ... Additional arguments (currently unused, for compatibility)
#' @export
icy_warn <- function(msg, ...) {
  warning(col_yellow(paste0("! ", msg)), call. = FALSE)
}

#' Information message
#'
#' @description
#' Displays an information message using base R message().
#' Plain text output, no special formatting.
#'
#' @param msg Information message
#' @param ... Additional arguments (currently unused, for compatibility)
#' @export
icy_inform <- function(msg, ...) {
  message(msg)
}

#' Plain text output
#'
#' @description
#' Outputs plain text using cat(). No special formatting or colors.
#'
#' @param msg Text to output
#' @param ... Additional arguments passed to cat()
#' @export
icy_text <- function(msg, ...) {
  cat(msg, "\n", sep = "")
}

#' Success alert message
#'
#' @description
#' Displays a success message with green checkmark when terminal supports it.
#'
#' @param msg Success message
#' @export
icy_alert_success <- function(msg) {
  message(col_green("✓ "), msg)
}

#' Warning alert message
#'
#' @description
#' Displays a warning message with yellow exclamation mark when terminal supports it.
#'
#' @param msg Warning message
#' @export
icy_alert_warning <- function(msg) {
  message(col_yellow("! "), msg)
}

#' Information alert message
#'
#' @description
#' Displays an information message with blue info symbol when terminal supports it.
#'
#' @param msg Information message
#' @export
icy_alert_info <- function(msg) {
  message(col_blue("i "), msg)
}

#' Danger alert message
#'
#' @description
#' Displays a danger/error message with red X mark when terminal supports it.
#'
#' @param msg Danger message
#' @export
icy_alert_danger <- function(msg) {
  message(col_red("✗ "), msg)
}

#' Bulleted list output
#'
#' @description
#' Simple implementation of bulleted list. Takes a named vector
#' and outputs with basic bullet points.
#'
#' @param items Named vector of items to display
#' @export
icy_bullets <- function(items) {
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
#' @export
icy_h3 <- function(msg) {
  message(col_bold(paste0("--- ", msg, " ---")))
}

#' Enhanced information message (contextual integration)
#'
#' @description
#' Enhanced version of icy_inform that uses contextual package
#' for rich formatting when available, falls back to basic message otherwise.
#'
#' @param msg Information message
#' @param ... Additional message components
#' @export
icy_enhanced_inform <- function(msg, ...) {
  if (requireNamespace("contextual", quietly = TRUE)) {
    contextual::cx_text(msg, ...)
  } else {
    icy_inform(msg, ...)
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
#' @export
icy_enhanced_alert <- function(msg, type = "info") {
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
      "info" = icy_alert_info(msg),
      "warning" = icy_alert_warning(msg),
      "success" = icy_alert_success(msg),
      "danger" = icy_alert_danger(msg),
      icy_alert_info(msg)
    )
  }
}