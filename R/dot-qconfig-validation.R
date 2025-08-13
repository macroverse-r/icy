#' qconfig Parameter Validation Utilities
#'
#' Internal functions for validating and normalizing parameters in the qconfig 
#' interactive configuration system.
#'
#' @name qconfig-validation
#' @keywords internal
NULL

#' Validate and Normalize qconfig Parameters
#'
#' Internal helper function to validate all qconfig parameters and normalize types.
#'
#' @param var_name Variable name
#' @param package Package name
#' @param section Section name
#' @param description Description text
#' @param options Option values
#' @param allow_skip Allow skip flag
#' @param note Note text
#' @param arg_only Argument only flag
#' @param write Write destination
#' @param type Variable type
#' @param allow_custom Allow custom input flag
#' @param allow_create_dir Allow directory creation flag
#' @param resolve_paths Path resolution mode
#' @param fn_tmpl Optional path to custom YAML template file
#' @param fn_local Optional path to custom local YAML file
#' @param verbose Verbose flag
#' @return List of validated and normalized parameters
#' @keywords internal
.validate_and_normalize_qconfig_params <- function(var_name, package, section, description, options, allow_skip, note, arg_only, write, type, allow_custom, allow_create_dir, resolve_paths, fn_tmpl, fn_local, verbose) {
  # Input validation
  if (!is.character(var_name) || length(var_name) != 1 || nchar(var_name) == 0) {
    .icy_stop("var_name must be a non-empty character string")
  }
  
  if (!is.character(package) || length(package) != 1 || nchar(package) == 0) {
    .icy_stop("package must be a non-empty character string")
  }
  
  if (!is.character(section) || length(section) != 1 || nchar(section) == 0) {
    .icy_stop("section must be a non-empty character string")
  }
  
  if (!is.null(description) && (!is.character(description) || length(description) != 1)) {
    .icy_stop("description must be NULL or a character string")
  }
  
  if (!is.null(options) && !is.vector(options)) {
    .icy_stop("options must be NULL or a vector")
  }
  
  if (!is.logical(allow_skip) || length(allow_skip) != 1 || is.na(allow_skip)) {
    .icy_stop("allow_skip must be TRUE or FALSE")
  }
  
  if (!is.null(note) && (!is.character(note) || length(note) != 1)) {
    .icy_stop("note must be NULL or a character string")
  }
  
  if (!is.logical(arg_only) || length(arg_only) != 1 || is.na(arg_only)) {
    .icy_stop("arg_only must be TRUE or FALSE")
  }
  
  if (!is.character(write) || length(write) != 1 || !write %in% c("local", "renviron", "session")) {
    .icy_stop("write must be one of: 'local', 'renviron', 'session'")
  }
  
  if (!is.null(type) && (!is.character(type) || length(type) != 1 || !type %in% c("character", "integer", "numeric", "logical", "boolean", "dir", "path"))) {
    .icy_stop("type must be NULL or one of: 'character', 'integer', 'numeric', 'logical', 'boolean', 'dir', 'path'")
  }
  
  if (!is.null(allow_custom) && (!is.logical(allow_custom) || length(allow_custom) != 1 || is.na(allow_custom))) {
    .icy_stop("allow_custom must be NULL, TRUE or FALSE")
  }
  
  if (!is.logical(allow_create_dir) || length(allow_create_dir) != 1 || is.na(allow_create_dir)) {
    .icy_stop("allow_create_dir must be TRUE or FALSE")
  }
  
  if (!is.character(resolve_paths) || length(resolve_paths) != 1 || !resolve_paths %in% c("ask", "static", "dynamic")) {
    .icy_stop("resolve_paths must be one of: 'ask', 'static', 'dynamic'")
  }
  
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    .icy_stop("verbose must be TRUE or FALSE")
  }
  
  # Validate fn_tmpl and fn_local consistency - both or neither
  if (is.null(fn_tmpl) && !is.null(fn_local)) {
    .icy_stop("fn_local requires fn_tmpl to be specified. Both parameters must be used together.")
  }
  if (!is.null(fn_tmpl) && is.null(fn_local)) {
    .icy_stop("fn_tmpl requires fn_local to be specified. Both parameters must be used together.")
  }
  
  # Validate fn_tmpl and fn_local types if specified
  if (!is.null(fn_tmpl) && (!is.character(fn_tmpl) || length(fn_tmpl) != 1 || nchar(fn_tmpl) == 0)) {
    .icy_stop("fn_tmpl must be a non-empty character string")
  }
  if (!is.null(fn_local) && (!is.character(fn_local) || length(fn_local) != 1 || nchar(fn_local) == 0)) {
    .icy_stop("fn_local must be a non-empty character string")
  }
  
  # Normalize type parameter (boolean logic moved to main function)
  if (!is.null(type) && type %in% c("boolean", "bool")) {
    type <- "logical"
  }
  if (!is.null(type) && type == "dir") {
    type <- "path"
  }
  
  return(list(
    var_name = var_name,
    package = package,
    section = section,
    description = description,
    options = options,
    allow_skip = allow_skip,
    note = note,
    arg_only = arg_only,
    write = write,
    type = type,
    allow_custom = allow_custom,
    allow_create_dir = allow_create_dir,
    resolve_paths = resolve_paths,
    fn_tmpl = fn_tmpl,
    fn_local = fn_local,
    verbose = verbose
  ))
}