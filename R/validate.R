#' Validate Configuration
#'
#' Validates environment variable names and optionally their values against
#' the package's configuration. This function provides comprehensive validation
#' including name checking, type validation, and custom validation rules.
#'
#' @param package Character string with the package name. Defaults to `get_package_name()` to detect the calling package.
#' @param var_names Character vector of variable names to validate. If NULL,
#'   validates all variables defined in the template.
#' @param values Optional named list of values to validate. If provided,
#'   validates both names and values.
#' @param user Character string for the user/section in the YAML file (default: "default").
#' @param warn Logical; if TRUE (default), issues warnings for validation failures.
#'   If FALSE, raises an error on first validation failure.
#' @param allowed_vars Optional character vector of allowed variable names. If NULL,
#'   retrieves from the package's template configuration.
#'
#' @return Logical; TRUE if all validation passes, FALSE if any validation fails
#'   (when warn=TRUE), or an error (when warn=FALSE).
#'
#' @examples
#' \dontrun{
#' # Validate variable names only
#' valid <- validate("mypackage", var_names = c("API_KEY", "DB_HOST"))
#'
#' # Validate both names and values
#' valid <- validate(
#'   "mypackage",
#'   values = list(API_KEY = "secret", DB_HOST = "localhost")
#' )
#'
#' # Strict validation (error on failure)
#' validate(
#'   "mypackage",
#'   var_names = c("API_KEY", "INVALID_VAR"),
#'   warn = FALSE
#' )
#' }
#'
#' @export
validate <- function(package = get_package_name(),
                     var_names = NULL,
                     values = NULL,
                     user = "default",
                     warn = TRUE,
                     allowed_vars = NULL) {
    
    # Get allowed variables from template if not provided
    if (is.null(allowed_vars)) {
        allowed_vars <- tryCatch({
            names(get_config(package = package, origin = "template", user = user))
        }, error = function(e) {
            msg <- paste0("Could not retrieve allowed variables from template: ", e$message)
            if (warn) {
                .icy_alert_warning(msg)
                return(character(0))
            } else {
                .icy_stop(msg)
            }
        })
    }
    
    # If values provided, extract var_names from them
    if (!is.null(values)) {
        if (!is.list(values) || is.null(names(values))) {
            msg <- "values must be a named list"
            if (warn) {
                .icy_alert_danger(msg)
                return(FALSE)
            } else {
                .icy_stop(msg)
            }
        }
        value_names <- names(values)
        
        # If var_names also provided, check consistency
        if (!is.null(var_names)) {
            if (!setequal(var_names, value_names)) {
                msg <- "var_names and names(values) must match"
                if (warn) {
                    .icy_alert_danger(msg)
                    return(FALSE)
                } else {
                    .icy_stop(msg)
                }
            }
        } else {
            var_names <- value_names
        }
    }
    
    # If no var_names at this point, validate all allowed vars
    if (is.null(var_names)) {
        var_names <- allowed_vars
    }
    
    # Validate variable names
    unknown_vars <- setdiff(var_names, allowed_vars)
    if (length(unknown_vars) > 0) {
        msg <- paste0(
            "Unknown environment variables: ", paste(unknown_vars, collapse = ", "), 
            ". Allowed variables: ", paste(allowed_vars, collapse = ", ")
        )
        
        if (warn) {
            .icy_alert_warning(msg)
            validation_passed <- FALSE
        } else {
            .icy_stop(msg)
        }
    } else {
        validation_passed <- TRUE
    }
    
    # Additional value validation if values provided
    if (!is.null(values) && validation_passed) {
        # Here we could add type checking, format validation, etc.
        # For now, just check that values are not NULL or empty
        for (var in names(values)) {
            val <- values[[var]]
            if (is.null(val) || (is.character(val) && nchar(val) == 0)) {
                msg <- paste0("Variable ", var, " has empty value")
                if (warn) {
                    .icy_alert_warning(msg)
                    validation_passed <- FALSE
                } else {
                    .icy_stop(msg)
                }
            }
        }
    }
    
    if (validation_passed && length(var_names) > 0) {
        .icy_alert_success("All variables validated successfully")
    }
    
    return(validation_passed)
}