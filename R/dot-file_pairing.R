#' Smart Template-Local File Pairing Utilities
#'
#' Internal functions for intelligent auto-detection and pairing of template/local 
#' configuration file pairs. These functions enable users to specify only one file
#' (template or local) and automatically detect or suggest the corresponding pair.
#'
#' @name smart-file-pairing
#' @keywords internal
NULL

#' Check File Existence with Package Context Awareness
#'
#' Checks if a file exists, considering package directory structure.
#' When in a package directory and given just a filename, also checks inst/.
#'
#' @param filename File path or name to check
#' @param package Package name for context
#' @param is_template Logical, TRUE for templates (check inst/), FALSE for locals
#' @return Full path to file if exists, NULL otherwise
#' @keywords internal
.check_file_exists <- function(filename, package, is_template = TRUE) {
  # If it's a full path or file exists as-is, return it
  if (file.exists(filename)) {
    return(normalizePath(filename, winslash = "/"))
  }
  
  # If it contains path separators, don't search elsewhere
  if (grepl("[/\\\\]", filename)) {
    return(NULL)
  }
  
  # Prepare list of files to check (with and without YAML extensions)
  files_to_check <- filename
  
  # If filename doesn't have an extension, try common YAML extensions
  if (!grepl("\\.(ya?ml)$", filename, ignore.case = TRUE)) {
    files_to_check <- c(filename, paste0(filename, ".yml"), paste0(filename, ".yaml"))
  }
  
  # Just a filename - check in appropriate directories
  if (is_template && .is_pkg_dir(package)) {
    # For templates in package dev, check inst/
    for (file_variant in files_to_check) {
      inst_path <- file.path("inst", file_variant)
      if (file.exists(inst_path)) {
        return(normalizePath(inst_path, winslash = "/"))
      }
    }
  }
  
  return(NULL)
}

#' Check File Pairing Status (Non-Exception Version)
#'
#' Checks the status of template/local file pairing without throwing exceptions.
#' This function is used for early detection of missing files before starting
#' interactive processes.
#'
#' @param fn_tmpl Character string with template file path/name (or NULL)
#' @param fn_local Character string with local file path/name (or NULL)
#' @param package Character string with package name
#' @param verbose Logical, whether to show detailed messages
#' @return List with status information: status, fn_tmpl, fn_local, missing_type, suggested_name
#' @keywords internal
.check_file_pairing <- function(fn_tmpl = NULL, fn_local = NULL, package, verbose = FALSE) {
  
  # First, validate existence of provided files
  if (!is.null(fn_tmpl)) {
    # Smart check for file existence
    exact_path <- .check_file_exists(fn_tmpl, package, is_template = TRUE)
    
    if (!is.null(exact_path)) {
      # Found file directly
      tmpl_status <- "exact_match"
      actual_tmpl <- exact_path
    } else {
      # Search in package directories
      found_tmpl <- .find_matching_pattern(
        package = package,
        fn_pattern = fn_tmpl,
        user_dir = FALSE,
        verbose = FALSE
      )
      
      if (length(found_tmpl) > 0) {
        # Check if any found file has exact same basename
        exact_matches <- found_tmpl[basename(found_tmpl) == basename(fn_tmpl)]
        
        if (length(exact_matches) > 0) {
          # Exact basename match - treat as exact match!
          tmpl_status <- "exact_match"
          actual_tmpl <- exact_matches[1]
          if (verbose && !grepl("[/\\\\]", fn_tmpl)) {
            .icy_text(paste0("Found ", basename(fn_tmpl), " in: ", dirname(exact_matches[1])))
          }
        } else {
          # True fuzzy match - different basename
          tmpl_status <- "fuzzy_match"
          actual_tmpl <- found_tmpl[1]
        }
      } else {
        # No match at all
        return(list(
          status = "template_not_found",
          fn_tmpl = fn_tmpl,
          fn_local = fn_local,
          missing_type = NULL,
          suggested_name = NULL
        ))
      }
    }
    
    # Only return fuzzy status for TRUE fuzzy matches
    if (tmpl_status == "fuzzy_match") {
      return(list(
        status = "template_fuzzy_match",
        fn_tmpl = fn_tmpl,           # Original input
        actual_tmpl = actual_tmpl,   # Fuzzy matched file
        fn_local = fn_local,
        missing_type = NULL,
        suggested_name = NULL
      ))
    }
  }
  
  if (!is.null(fn_local)) {
    # Smart check for file existence (locals don't check inst/)
    exact_path <- .check_file_exists(fn_local, package, is_template = FALSE)
    
    if (!is.null(exact_path)) {
      # Found file directly
      local_status <- "exact_match"
      actual_local <- exact_path
    } else {
      # Search in user config directories
      found_local <- .find_matching_pattern(
        package = package,
        fn_pattern = fn_local,
        user_dir = TRUE,
        verbose = FALSE
      )
      
      if (length(found_local) > 0) {
        # Check if any found file has exact same basename
        exact_matches <- found_local[basename(found_local) == basename(fn_local)]
        
        if (length(exact_matches) > 0) {
          # Exact basename match - treat as exact match!
          local_status <- "exact_match"
          actual_local <- exact_matches[1]
          if (verbose && !grepl("[/\\\\]", fn_local)) {
            .icy_text(paste0("Found ", basename(fn_local), " in: ", dirname(exact_matches[1])))
          }
        } else {
          # True fuzzy match - different basename
          local_status <- "fuzzy_match"
          actual_local <- found_local[1]
        }
      } else {
        # No match at all
        return(list(
          status = "local_not_found",
          fn_tmpl = fn_tmpl,
          fn_local = fn_local,
          missing_type = NULL,
          suggested_name = NULL
        ))
      }
    }
    
    # Only return fuzzy status for TRUE fuzzy matches  
    if (local_status == "fuzzy_match") {
      return(list(
        status = "local_fuzzy_match",
        fn_tmpl = fn_tmpl,
        fn_local = fn_local,           # Original input
        actual_local = actual_local,   # Fuzzy matched file
        missing_type = NULL,
        suggested_name = NULL
      ))
    }
  }
  
  # If both are provided and both exist, return as-is
  if (!is.null(fn_tmpl) && !is.null(fn_local)) {
    return(list(
      status = "both_provided",
      fn_tmpl = fn_tmpl,
      fn_local = fn_local,
      missing_type = NULL,
      suggested_name = NULL
    ))
  }
  
  # If neither are provided, use defaults
  if (is.null(fn_tmpl) && is.null(fn_local)) {
    return(list(
      status = "use_defaults",
      fn_tmpl = NULL,
      fn_local = NULL,
      missing_type = NULL,
      suggested_name = NULL
    ))
  }
  
  # Smart pairing case: one file provided, check for the other
  if (!is.null(fn_tmpl) && is.null(fn_local)) {
    # Template provided, find corresponding local
    result <- .find_corresponding_file(fn_tmpl, "local", package, user_dir = TRUE)
    if (length(result) > 0) {
      return(list(
        status = "pair_found",
        fn_tmpl = fn_tmpl,
        fn_local = result[1],
        missing_type = NULL,
        suggested_name = NULL
      ))
    } else {
      # Could not find corresponding local file
      suggested_name <- .generate_corresponding_file(fn_tmpl, "local")
      return(list(
        status = "missing_pair",
        fn_tmpl = fn_tmpl,
        fn_local = NULL,
        missing_type = "local",
        suggested_name = suggested_name
      ))
    }
  }
  
  if (is.null(fn_tmpl) && !is.null(fn_local)) {
    # Local provided, find corresponding template
    result <- .find_corresponding_file(fn_local, "template", package, user_dir = FALSE)
    if (length(result) > 0) {
      return(list(
        status = "pair_found",
        fn_tmpl = result[1],
        fn_local = fn_local,
        missing_type = NULL,
        suggested_name = NULL
      ))
    } else {
      # Could not find corresponding template file
      suggested_name <- .generate_corresponding_file(fn_local, "template")
      return(list(
        status = "missing_pair",
        fn_tmpl = NULL,
        fn_local = fn_local,
        missing_type = "template",
        suggested_name = suggested_name
      ))
    }
  }
}



#' Generate Corresponding Filename
#'
#' Takes a filename pattern and generates the corresponding filename by
#' replacing the type placeholder with the target type.
#'
#' @param filename Character string with original filename
#' @param target_type Character string, either "template" or "local"
#' @return Character string with corresponding filename
#' @keywords internal
.generate_corresponding_file <- function(filename, target_type) {
  
  # Instead of using a pattern with {type}, directly transform the filename
  # by replacing type indicators
  
  base_name <- tools::file_path_sans_ext(filename)
  extension <- tools::file_ext(filename)
  if (length(extension) == 0 || extension == "") extension <- "yml"  # default extension
  
  if (target_type == "local") {
    # Case-specific replacements FIRST (preserve case)
    # Use word boundaries to prevent double replacements
    new_base <- base_name
    if (grepl("Template", new_base)) {
      new_base <- gsub("Template", "Local", new_base)
    } else if (grepl("TEMPLATE", new_base)) {
      new_base <- gsub("TEMPLATE", "LOCAL", new_base)
    } else if (grepl("Tmpl", new_base)) {
      new_base <- gsub("Tmpl", "Local", new_base)
    } else if (grepl("TMPL", new_base)) {
      new_base <- gsub("TMPL", "LOCAL", new_base)
    } else {
      # Case-insensitive as fallback (only if no case-specific match)
      if (grepl("template", new_base, ignore.case = TRUE)) {
        new_base <- gsub("template", "local", new_base, ignore.case = TRUE)
      } else if (grepl("tmpl", new_base, ignore.case = TRUE)) {
        new_base <- gsub("tmpl", "local", new_base, ignore.case = TRUE)
      } else {
        # No template indicator found, append _local
        new_base <- paste0(base_name, "_local")
      }
    }
    
  } else if (target_type == "template") {
    # Case-specific replacements FIRST (preserve case)
    # Use word boundaries to prevent double replacements
    new_base <- base_name
    if (grepl("Local", new_base)) {
      new_base <- gsub("Local", "Template", new_base)
    } else if (grepl("LOCAL", new_base)) {
      new_base <- gsub("LOCAL", "TEMPLATE", new_base)
    } else if (grepl("Config", new_base)) {
      new_base <- gsub("Config", "Template", new_base)
    } else if (grepl("CONFIG", new_base)) {
      new_base <- gsub("CONFIG", "TEMPLATE", new_base)
    } else {
      # Case-insensitive as fallback (only if no case-specific match)
      if (grepl("local", new_base, ignore.case = TRUE)) {
        new_base <- gsub("local", "template", new_base, ignore.case = TRUE)
      } else if (grepl("config", new_base, ignore.case = TRUE)) {
        new_base <- gsub("config", "template", new_base, ignore.case = TRUE)
      } else {
        # No indicator found, append _template
        new_base <- paste0(base_name, "_template")
      }
    }
  }
  
  return(paste0(new_base, ".", extension))
}

#' Find Corresponding File Using Existing Infrastructure
#'
#' Searches for a corresponding file using the existing .find_matching_pattern()
#' infrastructure, which handles directory searching and pattern matching.
#'
#' @param filename Character string with the original filename
#' @param target_type Character string, "template" or "local"
#' @param package Character string with package name
#' @param user_dir Logical, whether to search in user directory
#' @return Character vector of matching file paths
#' @keywords internal
.find_corresponding_file <- function(filename, target_type, package, user_dir) {
  
  # Generate the corresponding filename to search for
  corresponding_name <- .generate_corresponding_file(filename, target_type)
  
  # Use existing file discovery infrastructure
  matches <- .find_matching_pattern(
    package = package,
    fn_pattern = corresponding_name,
    user_dir = user_dir,
    verbose = FALSE
  )
  
  return(matches)
}


#' Interactive File Creation Helper
#'
#' Handles interactive creation of missing local configuration files
#' with user confirmation. Used by both setup() and qconfig().
#'
#' @param missing_file Character string with the name of the missing file
#' @param provided_file Character string with the name of the provided file  
#' @param missing_type Character string, "local" or "template"
#' @param package Character string with package name
#' @param section Character string with section name
#' @param fn_tmpl Character string with template file name (for create_local)
#' @param verbose Logical, whether to show verbose messages
#' @return Character string with created file name, or NULL if creation declined
#' @keywords internal
.interactive_file_creation <- function(missing_file, provided_file, missing_type, package, section, fn_tmpl = NULL, verbose = FALSE) {
  
  missing_type_text <- if (missing_type == "local") "Local" else "Template"
  provided_type_text <- if (missing_type == "local") "template" else "local"
  
  .icy_alert(paste0(missing_type_text, " file '", missing_file, "' not found for ", provided_type_text, " '", provided_file, "'"))
  
  # Interactive creation prompt
  create_prompt <- paste0("Create ", tolower(missing_type_text), " config file now? (Y/n): ")
  user_input <- readline(create_prompt)
  
  if (tolower(trimws(user_input)) %in% c("", "y", "yes")) {
    # Create the missing file
    tryCatch({
      if (missing_type == "local") {
        created_path <- create_local(
          package = package,
          fn_local = missing_file,
          fn_tmpl = fn_tmpl,
          tmpl_section = section,
          overwrite = FALSE,
          verbose = verbose
        )
        .icy_success(paste0("Created '", missing_file, "' from template"))
        return(missing_file)
      } else {
        .icy_stop("Cannot automatically create template files. Template files should be provided by the package.")
      }
    }, error = function(e) {
      .icy_stop(paste0("Failed to create ", tolower(missing_type_text), " file: ", e$message))
    })
  } else {
    return(NULL)  # User declined creation
  }
}

#' Interactive Fuzzy Match Confirmation
#'
#' Asks user to confirm using a fuzzy-matched file when exact match was not found.
#' Used when misspelled filenames find similar files via pattern matching.
#'
#' @param original_input Character string with the user's original input
#' @param fuzzy_match Character string with the fuzzy-matched filename
#' @param file_type Character string, "template" or "local"
#' @return Character string with confirmed filename, or NULL if user declines
#' @keywords internal
.confirm_fuzzy_match <- function(original_input, fuzzy_match, file_type) {
  
  fuzzy_basename <- basename(fuzzy_match)
  .icy_alert(paste0("No exact match for '", original_input, "'. Found '", fuzzy_basename, "'. Use this instead?"))
  
  # Interactive confirmation prompt
  confirm_prompt <- "Continue with fuzzy match? (Y/n): "
  user_input <- readline(confirm_prompt)
  
  if (tolower(trimws(user_input)) %in% c("", "y", "yes")) {
    .icy_success(paste0("Using '", fuzzy_basename, "'"))
    return(fuzzy_match)
  } else {
    .icy_inform(paste0("Fuzzy match declined for ", file_type, " file"))
    return(NULL)
  }
}

#' Unified File Validation and Pairing
#'
#' Comprehensive file validation that handles existence checks, pairing logic,
#' and interactive file creation. Used by both setup() and qconfig() for
#' consistent behavior.
#'
#' @param fn_tmpl Character string with template file path/name (or NULL)
#' @param fn_local Character string with local file path/name (or NULL)
#' @param package Character string with package name
#' @param section Character string with section name  
#' @param verbose Logical, whether to show detailed messages
#' @return List with validated file paths and status, or throws error for missing files
#' @keywords internal
.validate_file_pairing <- function(fn_tmpl = NULL, fn_local = NULL, package, section = "default", verbose = FALSE) {
  
  # Check file pairing status
  pairing_status <- .check_file_pairing(fn_tmpl, fn_local, package, verbose)
  
  # Handle file not found errors (always fatal)
  if (pairing_status$status == "template_not_found") {
    .icy_stop(paste0("Template file '", fn_tmpl, "' not found"))
  }
  
  if (pairing_status$status == "local_not_found") {
    .icy_stop(paste0("Local file '", fn_local, "' not found"))
  }
  
  # Handle fuzzy matches with user confirmation
  if (pairing_status$status == "template_fuzzy_match") {
    confirmed_tmpl <- .confirm_fuzzy_match(
      original_input = pairing_status$fn_tmpl,
      fuzzy_match = pairing_status$actual_tmpl,
      file_type = "template"
    )
    
    if (is.null(confirmed_tmpl)) {
      .icy_stop("Template file selection cancelled")
    } else {
      # Update fn_tmpl to use the confirmed fuzzy match
      fn_tmpl <- confirmed_tmpl
      # Re-check pairing with the confirmed template
      pairing_status <- .check_file_pairing(fn_tmpl, fn_local, package, verbose)
    }
  }
  
  if (pairing_status$status == "local_fuzzy_match") {
    confirmed_local <- .confirm_fuzzy_match(
      original_input = pairing_status$fn_local,
      fuzzy_match = pairing_status$actual_local,
      file_type = "local"
    )
    
    if (is.null(confirmed_local)) {
      .icy_stop("Local file selection cancelled")
    } else {
      # Update fn_local to use the confirmed fuzzy match
      fn_local <- confirmed_local
      # Re-check pairing with the confirmed local file
      pairing_status <- .check_file_pairing(fn_tmpl, fn_local, package, verbose)
    }
  }
  
  # Handle missing pair case - always interactive
  if (pairing_status$status == "missing_pair") {
    # Offer interactive creation
    missing_file <- pairing_status$suggested_name
    provided_file <- if (pairing_status$missing_type == "local") fn_tmpl else fn_local
    
    created_file <- .interactive_file_creation(
      missing_file = missing_file,
      provided_file = provided_file,
      missing_type = pairing_status$missing_type,
      package = package,
      section = section,
      fn_tmpl = fn_tmpl,
      verbose = verbose
    )
    
    if (is.null(created_file)) {
      .icy_stop("Setup cancelled - missing required files")
    } else {
      # Update the pairing status with the newly created file
      if (pairing_status$missing_type == "local") {
        fn_local <- created_file
      } else {
        fn_tmpl <- created_file
      }
      return(list(fn_tmpl = fn_tmpl, fn_local = fn_local, source = "created"))
    }
  }
  
  # For other statuses (both_provided, use_defaults, pair_found), return the results
  return(list(
    fn_tmpl = pairing_status$fn_tmpl,
    fn_local = pairing_status$fn_local,
    source = pairing_status$status
  ))
}

#' List Available Files of Specified Type
#'
#' Lists all available template or local files in the appropriate directory
#' using existing file discovery infrastructure.
#'
#' @param file_type Character string, "template" or "local"
#' @param package Character string with package name  
#' @param user_dir Logical, whether to search in user directory
#' @return Character vector of available file paths
#' @keywords internal
.list_available_files <- function(file_type, package, user_dir) {
  
  # Create broad pattern to find files containing type indicators
  if (file_type == "template") {
    pattern <- "(template|Template|TEMPLATE|tmpl|Tmpl|TMPL)"
  } else if (file_type == "local") {
    pattern <- "(local|Local|LOCAL|config|Config|CONFIG)"
  } else {
    return(character(0))
  }
  
  # Use existing infrastructure to find files
  all_files <- .find_matching_pattern(
    package = package,
    fn_pattern = pattern,
    user_dir = user_dir,
    verbose = FALSE
  )
  
  return(all_files)
}