
.onLoad <- function(libname, pkgname) {
  # Get paths to the template and local config files
  # local_config_file <- get_config_path(package = pkgname,
  #                                           tmpl2local_comp = "default")
  # print(local_config_file)

#   # Load the local configuration
#   config <- yaml::read_yaml(file = local_config_file)
#   
#   # Set environment variables based on configuration
#   # (with defaults if not set)
#   Sys.setenv(MSGM_DIR = config$MSGM_DIR %||% getwd())
#   
#   Sys.setenv(MSGM_RESULT_DIR = config$MSGM_RESULT_DIR %||%
#                file.path(Sys.getenv("MSGM_DIR"), "results"))
#
#   Sys.setenv(MSGM_INTERMEDIATE_DIR = config$MSGM_INTERMEDIATE_DIR %||%
#     if (.Platform$OS.type == "windows") {
#       Sys.getenv("TEMP", unset = file.path(Sys.getenv("MSGM_DIR"), "temp"))
#     } else {
#       "/tmp"
#     })
#
#   # Set MSGM_REPORTING_DIR only if it exists in config and is not empty
#   if (!is.null(config$MSGM_REPORTING_DIR) && nzchar(config$MSGM_REPORTING_DIR)) {
#     Sys.setenv(MSGM_REPORTING_DIR = config$MSGM_REPORTING_DIR)
#   }
#
#   # Set PIK_USERNAME only if it exists in config and is not empty
#   if (!is.null(config$PIK_USERNAME) && nzchar(config$PIK_USERNAME)) {
#     Sys.setenv(PIK_USERNAME = config$PIK_USERNAME)
#   }
#
#   Sys.setenv(MSGM_VERBOSE = config$MSGM_VERBOSE %||% TRUE)
#   Sys.setenv(MSGM_DEBUG = config$MSGM_DEBUG %||% FALSE)
# }
#
# .onAttach <- function(libname, pkgname) {
#   local_config_file <- file.path(system.file(package = pkgname), "msgm_config_local.yml")
#   packageStartupMessage("MSGM configuration loaded from: ", local_config_file)
#
#   # Check if required directories exist and warn if not
#   check_directory <- function(env_var, description, skip_check = FALSE) {
#     dir_path <- Sys.getenv(env_var, unset = "")
#     
#     # Skip the check if requested
#     if (skip_check) {
#       return()
#     }
#     
#     if (nzchar(dir_path) && !dir.exists(dir_path)) {
#       message(sprintf("WARNING: %s directory does not exist: %s", 
#                       description, dir_path))
#     }
#   }
#
#   # Get current MSGM_DIR value
#   msgm_dir <- Sys.getenv("MSGM_DIR", unset = "")
#   
#   # Check if MSGM_DIR is the current directory or the package name
#   is_current_dir <- identical(normalizePath(msgm_dir, mustWork = FALSE), 
#                              normalizePath(getwd(), mustWork = FALSE))
#   
#   # Get the dir name of MSGM_DIR
#   msgm_dir_name <- basename(msgm_dir)
#   is_package_dir <- identical(msgm_dir_name, pkgname)
#   
#   # Determine if we should skip warnings for MSGM_RESULT_DIR
#   skip_result_warning <- is_current_dir || is_package_dir
#   
#   # Check config directories
#   check_directory("MSGM_DIR", "Main MSGM")
#   check_directory("MSGM_RESULT_DIR", "Results", skip_check = skip_result_warning)
#   check_directory("MSGM_REPORTING_DIR", "Reporting")
#   check_directory("MSGM_INTERMEDIATE_DIR", "Temporary files")
}
