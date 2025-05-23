#' Find Environment Variables YAML Configuration File
#'
#' Locates the YAML configuration file that contains environment variable definitions
#' for a specified package. The function searches within the package's installation directory
#' (or development directory for packages loaded with devtools), including all subdirectories.
#'
#' This function is primarily designed for package developers who want to define and manage
#' environment variables for their package in a structured way. It supports different naming
#' conventions for the YAML files to accommodate various coding styles.
#'
#' The function automatically handles the difference between installed packages and packages
#' loaded via devtools during development.
#'
#' @param package Character string with the package name. If NULL (default),
#'   the function will automatically use the `yml2env` as the current package name.
#' @param case_format Character string indicating the case format to use for the filename search.
#'   Options are:
#'   \itemize{
#'     \item "snake_case" (default): Searches for files like "package_env_vars.yml" or "*_env_vars.yml"
#'     \item "camelCase" or "PascalCase": Searches for files like "packageEnvVars.yml" or "*EnvVars.yml"
#'     \item "kebab-case": Searches for files like "package-env-vars.yml" or "*-env-vars.yml"
#'   }
#'
#' @return Character string with the full path to the found YAML file.
#' @examples
#' \dontrun{
#' # Find the environment variables YAML file for a package
#' yaml_path <- get_config_path("mypackage")
#'
#' # Read and process the YAML file
#' if (file.exists(yaml_path)) {
#'   yaml_content <- yaml::read_yaml(yaml_path)
#'   print(yaml_content$environment_variables)
#' }
#'
#' # Using a different naming convention
#' yaml_path <- get_config_path("mypackage", case_format = "camelCase")
#' }
#'
#' @export
get_config_path <- function(package = NULL,
                            fn_local = NULL,
                            fn_tmpl = NULL,
                            tmpl2local_comp = NULL,
                            case_format = "snake_case",
                            debug = FALSE,
                            force = FALSE) {

  # current function
  fun <- .cur_fun()

  # Use this package name (yml2env) if not provided
  if (is.null(package)) {
    package <- get_package_name()
  }

  if (debug) {
    cli::cli_text("From {.strong {fun}}: pkgname = {.path {package}}")
  }

  # Get paths to the template and local config files
  if (is.null(fn_local)) {
    fn_local_pattern <- .pattern(package = package, case_format = case_format, file = "local")
  } else {
    fn_local_pattern <- fn_local
  }

  if (debug) {
    cli::cli_text("From {.strong {fun}}: fn_local_pattern = {.path {fn_local_pattern}}")
  }

  # Find all .yml or .yaml files matching the pattern
  matching_file <- .find_matching_pattern(
    package = package,
    fn_pattern = fn_local_pattern,
    user_dir = TRUE,
    verbose = TRUE
  )

  if (debug) {
    cli::cli_text("From {.strong {fun}}: length(matching_file) = {length(matching_file)}")
  }


  # Create local config file if it doesn't exist
  if (length(matching_file) == 0) {
    if (force) {
      local_path <- .create_local(package = package,
                                  fn_local = fn_local,
                                  fn_tmpl = fn_tmpl,
                                  tmpl2local_comp = tmpl2local_comp,
                                  case_format = case_format,
                                  debug = debug)
    } else {
      cli::cli_abort("No local config YAML file could be found and force is FALSE.")
    }
    return(local_path)
  } else if (length(matching_file) == 1) {
    return(matching_file)
  } else {
    return(NULL)
  }
}


.create_local <- function(package,
                          fn_local = NULL,
                          fn_tmpl = NULL,
                          tmpl2local_comp = NULL,
                          case_format = "snake_case",
                          debug = FALSE) {

  # current function
  fun <- .cur_fun()

  # Use default naming convention if fn_local is not specified
  if (is.null(fn_local)) {
    fn_local <- .pattern(
      package = package,
      case_format = case_format,
      file = "local",
      yml = TRUE
    )
  }

  existing <- .find_matching_pattern(package = package,
                                     fn_pattern = fn_local,
                                     user_dir = TRUE,
                                     verbose = FALSE)

  if (length(existing) > 0) {
    cli::cli_abort("Local config YAML file already exist.")
  }

  if (!is.null(fn_tmpl)) {
    # Add path to template if not provided
    if (!grepl("[/\\\\]", fn_tmpl)) {
      fn_tmpl_path <- .find_matching_pattern(package = package,
                                             fn_pattern = fn_tmpl,
                                             user_dir = FALSE)
    } else {
      fn_tmpl_path <- fn_tmpl
    }
  } else {
    # Read template if not provided
    tmpl_pattern <- .pattern(package = package, case_format = case_format, file = "template")
    fn_tmpl_path <- .find_matching_pattern(package = package,
                                           fn_pattern = tmpl_pattern,
                                           user_dir = FALSE)

    if (length(fn_tmpl_path) != 1) {
      cli::cli_abort("No local YAML file found and no clear and unique template config YAML found.")
    } else {
      cli::cli_inform("Use {.file {fn_tmpl_path}} to create local config file.")
    }

  }

  if (debug) {
    cli::cli_text("From {.strong {fun}}: fn_tmpl_path = {.path {fn_tmpl_path}}")
  }

  # Read template and extract the relevant section
  template_config <- yaml::read_yaml(fn_tmpl_path)
  if (!is.null(tmpl2local_comp)) {
    local_config <- template_config[[tmpl2local_comp]]
  } else {
    # Only keep the default section for the local config
    local_config <- list()
    local_config$default <- template_config$default
  }

  # Add path to template if not provided
  if (!grepl("[/\\\\]", fn_local)) {
    fn_local_path <- file.path(get_package_dir(package = package), fn_local)
  } else {
    fn_local_path <- fn_tmpl
  }

  if (debug) {
    cli::cli_text("From {.strong {fun}}:")
    cli::cli_inform(" - fn_local = {fn_local}")
    cli::cli_inform(" - get_package_dir = {get_package_dir(package = package)}")
    cli::cli_inform(" - fn_local_path = {fn_local_path}")
  }

  # Create the directory if it doesn't exist
  if (!dir.exists(dirname(fn_local_path))) {
    dir.create(dirname(fn_local_path),
               recursive = TRUE,
               showWarnings = FALSE)
  }

  # Write just the default section to the local config file
  yaml::write_yaml(local_config, fn_local_path)

  return(fn_local_path)
}


# filename <- gsub("\\\\\\.|ya\\?ml|\\$", c(".", "yml", ""), pattern)

.find_matching_pattern <- function(package,
                                   fn_pattern,
                                   user_dir = TRUE,
                                   verbose = FALSE) {
  # Get the package path
  tryCatch(
    {
      package_dir <- get_package_dir(package = package,
                                     user_dir = user_dir)
    },
    error = function(e) {
      stop("Error locating package path: ", e$message)
    }
  )

  # Use list.files to recursively find yaml files
  yaml_files <- list.files(
    path = package_dir,
    pattern = "\\.ya?ml$",
    recursive = TRUE,
    full.names = TRUE
  )

  # Filter for files matching our pattern
  matching_files <- yaml_files[grepl(fn_pattern, yaml_files, ignore.case = TRUE)]

  if (verbose) {
    if (length(matching_files) == 0) {
      cli::cli_alert_warning("No YAML file in {.file {package_dir}} matching {.var {fn_pattern}}")
    } else if (length(matching_files) > 1) {
      cli::cli_alert_warning("Multiple config YAML files found: {.file {basename(matching_files)}}. \nPlease ensure only one file is present.")
    }
  }

  return(as.character(matching_files))
}
