#' File Finding Helper Functions
#'
#' Internal functions supporting find_config_files with a clean, simple structure.
#' These functions handle the core logic of searching, matching, and pairing
#' configuration files.
#'
#' @name file-finding-helpers
#' @keywords internal
NULL

#' Core File Finding Logic
#'
#' Searches for template and/or local configuration files and ensures pairing.
#' Always returns both files or NULL for each.
#'
#' @param fn_tmpl Character string with template filename to search for (or NULL)
#' @param fn_local Character string with local filename to search for (or NULL)
#' @param package Character string with package name
#' @param case_format Character string for default filename generation
#' @param fuzzy Logical. If TRUE, allows fuzzy matching
#' @param verbose Logical. If TRUE, shows detailed messages
#' @return List with:
#'   - fn_tmpl: Path to template file or NULL
#'   - fn_local: Path to local file or NULL
#'   - tmpl_fuzzy: TRUE if template was fuzzy matched
#'   - local_fuzzy: TRUE if local was fuzzy matched
#' @keywords internal
._find_files_core <- function(fn_tmpl = NULL,
                              fn_local = NULL,
                              package,
                              case_format = "snake_case",
                              fuzzy = TRUE,
                              verbose = FALSE) {
  
  # Initialize result
  result <- list(
    fn_tmpl = NULL,
    fn_local = NULL,
    tmpl_fuzzy = FALSE,
    local_fuzzy = FALSE
  )
  
  # If neither provided, use default patterns
  if (is.null(fn_tmpl) && is.null(fn_local)) {
    fn_tmpl <- .pattern(package = package, case_format = case_format, file = "template", yml = TRUE)
    fn_local <- .pattern(package = package, case_format = case_format, file = "local", yml = TRUE)
  }
  
  # Search for template
  if (!is.null(fn_tmpl)) {
    tmpl_search <- ._search_file(
      filename = fn_tmpl,
      package = package,
      type = "template",
      fuzzy = fuzzy,
      verbose = verbose
    )
    
    if (!is.null(tmpl_search$path)) {
      result$fn_tmpl <- tmpl_search$path
      result$tmpl_fuzzy <- tmpl_search$fuzzy
      
      # If no local was requested, generate paired name
      if (is.null(fn_local)) {
        fn_local <- .generate_corresponding_file(basename(tmpl_search$path), "local")
      }
    }
  }
  
  # Search for local
  if (!is.null(fn_local)) {
    local_search <- ._search_file(
      filename = fn_local,
      package = package,
      type = "local",
      fuzzy = fuzzy,
      verbose = verbose
    )
    
    if (!is.null(local_search$path)) {
      result$fn_local <- local_search$path
      result$local_fuzzy <- local_search$fuzzy
      
      # If no template was requested, generate paired name
      if (is.null(fn_tmpl) && is.null(result$fn_tmpl)) {
        fn_tmpl <- .generate_corresponding_file(basename(local_search$path), "template")
        
        # Try to find the paired template
        tmpl_search <- ._search_file(
          filename = fn_tmpl,
          package = package,
          type = "template",
          fuzzy = fuzzy,
          verbose = FALSE  # Don't be verbose for auto-pairing
        )
        
        if (!is.null(tmpl_search$path)) {
          result$fn_tmpl <- tmpl_search$path
          result$tmpl_fuzzy <- tmpl_search$fuzzy
        }
      }
    }
  }
  
  return(result)
}

#' Search for a Single File
#'
#' Searches for a file using exact matching first, then fuzzy matching if enabled.
#'
#' @param filename Character string with file path/name to search
#' @param package Character string with package name
#' @param type Character string: "template" or "local"
#' @param fuzzy Logical. If TRUE, allows fuzzy matching
#' @param verbose Logical. If TRUE, shows detailed messages
#' @return List with:
#'   - path: Full path to file or NULL
#'   - fuzzy: TRUE if found via fuzzy matching, FALSE otherwise
#' @keywords internal
._search_file <- function(filename, package, type = "template", fuzzy = TRUE, verbose = FALSE) {
  
  # First, check if it's a full path that exists
  if (grepl("[/\\\\]", filename)) {
    if (file.exists(filename) && !dir.exists(filename) && 
        grepl("\\.ya?ml$", filename, ignore.case = TRUE)) {
      return(list(path = normalizePath(filename, winslash = "/"), fuzzy = FALSE))
    }
  }
  
  # For templates in package dev mode, check inst/ directory
  if (type == "template" && .is_pkg_dir(package)) {
    # Try with and without yaml extensions
    files_to_check <- filename
    if (!grepl("\\.(ya?ml)$", filename, ignore.case = TRUE)) {
      files_to_check <- c(filename, paste0(filename, ".yml"), paste0(filename, ".yaml"))
    }
    
    for (file_variant in files_to_check) {
      inst_path <- file.path("inst", file_variant)
      if (file.exists(inst_path) && !dir.exists(inst_path)) {
        return(list(path = normalizePath(inst_path, winslash = "/"), fuzzy = FALSE))
      }
    }
  }
  
  # Get search directory
  package_dir <- get_config_dir(package = package, type = type)
  
  # Find all YAML files
  yaml_files <- list.files(
    path = package_dir,
    pattern = "\\.ya?ml$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Filter out directories
  yaml_files <- yaml_files[!dir.exists(yaml_files)]
  
  # For template searches, exclude local_config subdirectories
  if (type == "template") {
    yaml_files <- yaml_files[!grepl("/local_config/", yaml_files)]
  }
  
  # Check for exact basename match first
  exact_matches <- yaml_files[basename(yaml_files) == basename(filename)]
  if (length(exact_matches) > 0) {
    if (verbose && !grepl("[/\\\\]", filename)) {
      .icy_text(paste0("Found ", basename(filename), " in: ", dirname(exact_matches[1])))
    }
    return(list(path = exact_matches[1], fuzzy = FALSE))
  }
  
  # If fuzzy matching is disabled, stop here
  if (!fuzzy) {
    if (verbose) {
      .icy_warn(paste0("No exact match for '", filename, "' in ", package_dir))
    }
    return(list(path = NULL, fuzzy = FALSE))
  }
  
  # Try fuzzy matching
  similarities <- sapply(yaml_files, function(f) {
    ._calculate_filename_similarity(filename, f, package = package)
  })
  
  # Apply discrimination boost for better fuzzy matching
  boosted_similarities <- ._boost_similarities(similarities, yaml_files)
  
  # Find matches above threshold
  good_matches <- yaml_files[boosted_similarities >= 0.4]
  
  if (length(good_matches) > 0) {
    # Sort by similarity score
    good_matches <- good_matches[order(boosted_similarities[boosted_similarities >= 0.4], decreasing = TRUE)]
    
    if (verbose) {
      .icy_alert(paste0("No exact match for '", filename, "'. Found fuzzy match: ", basename(good_matches[1])))
    }
    
    return(list(path = good_matches[1], fuzzy = TRUE))
  }
  
  # No matches found
  if (verbose) {
    .icy_warn(paste0("No file matching '", filename, "' in ", package_dir))
  }
  
  return(list(path = NULL, fuzzy = FALSE))
}

#' Handle Fuzzy Match Confirmation
#'
#' Handles user confirmation for fuzzy matched files and ensures proper pairing
#' after confirmation.
#'
#' @param results List from ._find_files_core()
#' @param package Character string with package name
#' @param verbose Logical. If TRUE, shows detailed messages
#' @return Updated results list with user confirmations applied
#' @keywords internal
._handle_fuzzy_confirmation <- function(results, package, verbose = FALSE) {
  
  # Handle template fuzzy match confirmation
  if (results$tmpl_fuzzy && !is.null(results$fn_tmpl)) {
    confirmed <- .confirm_fuzzy_match(
      original_input = "template file",
      fuzzy_match = results$fn_tmpl,
      file_type = "template"
    )
    
    if (!confirmed) {
      results$fn_tmpl <- NULL
      results$tmpl_fuzzy <- FALSE
    } else {
      # After confirming template, ensure we have its pair
      if (is.null(results$fn_local)) {
        local_name <- .generate_corresponding_file(basename(results$fn_tmpl), "local")
        local_search <- ._search_file(
          filename = local_name,
          package = package,
          type = "local",
          fuzzy = FALSE,  # Use exact matching for auto-pairing
          verbose = FALSE
        )
        if (!is.null(local_search$path)) {
          results$fn_local <- local_search$path
          results$local_fuzzy <- FALSE
        }
      }
    }
  }
  
  # Handle local fuzzy match confirmation
  if (results$local_fuzzy && !is.null(results$fn_local)) {
    confirmed <- .confirm_fuzzy_match(
      original_input = "local file",
      fuzzy_match = results$fn_local,
      file_type = "local"
    )
    
    if (!confirmed) {
      results$fn_local <- NULL
      results$local_fuzzy <- FALSE
    } else {
      # After confirming local, ensure we have its pair
      if (is.null(results$fn_tmpl)) {
        tmpl_name <- .generate_corresponding_file(basename(results$fn_local), "template")
        tmpl_search <- ._search_file(
          filename = tmpl_name,
          package = package,
          type = "template",
          fuzzy = FALSE,  # Use exact matching for auto-pairing
          verbose = FALSE
        )
        if (!is.null(tmpl_search$path)) {
          results$fn_tmpl <- tmpl_search$path
          results$tmpl_fuzzy <- FALSE
        }
      }
    }
  }
  
  return(results)
}

#' Boost Similarity Scores
#'
#' Applies discrimination boosting to similarity scores to improve fuzzy matching.
#'
#' @param similarities Numeric vector of similarity scores
#' @param yaml_files Character vector of file paths
#' @return Numeric vector of boosted similarity scores
#' @keywords internal
._boost_similarities <- function(similarities, yaml_files) {
  boosted <- similarities
  
  if (length(similarities) > 1) {
    for (i in seq_along(yaml_files)) {
      this_score <- similarities[i]
      if (this_score > 0) {
        other_scores <- similarities[-i]
        if (length(other_scores) > 0) {
          next_best <- max(other_scores)
          avg_others <- mean(other_scores[other_scores > 0])
          
          if (next_best > 0) {
            discrimination_ratio <- this_score / next_best
            if (discrimination_ratio > 1) {
              boost_factor <- sqrt(discrimination_ratio)
              if (!is.na(avg_others) && avg_others > 0) {
                avg_ratio <- this_score / avg_others
                avg_boost <- 1 + (avg_ratio - 1) * 0.5
                boost_factor <- boost_factor * min(avg_boost, 2.5)
              }
              boosted[i] <- min(1, similarities[i] * boost_factor)
            }
          }
        }
      }
    }
  }
  
  return(boosted)
}

#' Calculate String Similarity for Fuzzy Matching
#'
#' Uses dual scoring approach to find similar filenames.
#'
#' @param pattern The pattern to search for (user input)
#' @param candidate A candidate filename to compare against
#' @param package Package name to remove from comparison (optional)
#' @return Numeric similarity score between 0 and 1
#' @keywords internal
._calculate_filename_similarity <- function(pattern, candidate, package = NULL) {
  # Work with basenames only
  pattern_base <- basename(pattern)
  candidate_base <- basename(candidate)
  
  # Remove extensions for comparison
  pattern_no_ext <- sub("\\.(ya?ml)$", "", pattern_base, ignore.case = TRUE)
  candidate_no_ext <- sub("\\.(ya?ml)$", "", candidate_base, ignore.case = TRUE)
  
  # Case-insensitive comparison
  pattern_lower <- tolower(pattern_no_ext)
  candidate_lower <- tolower(candidate_no_ext)
  
  # Perfect match
  if (pattern_lower == candidate_lower) {
    return(1.0)
  }
  
  # Calculate full string similarity
  edit_dist_full <- utils::adist(pattern_lower, candidate_lower)[1, 1]
  max_len_full <- max(nchar(pattern_lower), nchar(candidate_lower))
  similarity_full <- if (max_len_full > 0) 1 - (edit_dist_full / max_len_full) else 0
  
  # Generic terms to remove for content similarity
  generic_terms <- c("template", "tmpl", "local", "config", "cfg", "conf")
  if (!is.null(package) && nchar(package) > 0) {
    generic_terms <- c(generic_terms, tolower(package))
  }
  
  # Remove generic terms
  remove_generic <- function(str) {
    parts <- unlist(strsplit(str, "[_.-]"))
    parts_filtered <- parts[!tolower(parts) %in% generic_terms]
    if (length(parts_filtered) > 0) {
      paste(parts_filtered, collapse = "_")
    } else {
      ""
    }
  }
  
  pattern_stripped <- remove_generic(pattern_lower)
  candidate_stripped <- remove_generic(candidate_lower)
  
  # Calculate content similarity
  if (pattern_stripped == "" && candidate_stripped == "") {
    similarity_stripped <- similarity_full
  } else if (pattern_stripped == "" || candidate_stripped == "") {
    similarity_stripped <- 0
  } else if (pattern_stripped == candidate_stripped) {
    similarity_stripped <- 1.0
  } else {
    edit_dist_stripped <- utils::adist(pattern_stripped, candidate_stripped)[1, 1]
    max_len_stripped <- max(nchar(pattern_stripped), nchar(candidate_stripped))
    similarity_stripped <- if (max_len_stripped > 0) 1 - (edit_dist_stripped / max_len_stripped) else 0
  }
  
  # Return mean of both similarities
  return((similarity_full + similarity_stripped) / 2)
}

#' Generate Corresponding Filename
#'
#' Takes a filename and generates the corresponding paired filename.
#'
#' @param filename Character string with original filename
#' @param target_type Character string, either "template" or "local"
#' @return Character string with corresponding filename
#' @keywords internal
.generate_corresponding_file <- function(filename, target_type) {
  
  base_name <- tools::file_path_sans_ext(filename)
  extension <- tools::file_ext(filename)
  if (length(extension) == 0 || extension == "") extension <- "yml"
  
  if (target_type == "local") {
    # Replace template indicators with local
    new_base <- base_name
    if (grepl("Template", new_base)) {
      new_base <- gsub("Template", "Local", new_base)
    } else if (grepl("TEMPLATE", new_base)) {
      new_base <- gsub("TEMPLATE", "LOCAL", new_base)
    } else if (grepl("Tmpl", new_base)) {
      new_base <- gsub("Tmpl", "Local", new_base)
    } else if (grepl("TMPL", new_base)) {
      new_base <- gsub("TMPL", "LOCAL", new_base)
    } else if (grepl("template", new_base, ignore.case = TRUE)) {
      new_base <- gsub("template", "local", new_base, ignore.case = TRUE)
    } else if (grepl("tmpl", new_base, ignore.case = TRUE)) {
      new_base <- gsub("tmpl", "local", new_base, ignore.case = TRUE)
    } else {
      # No template indicator found, append _local
      new_base <- paste0(base_name, "_local")
    }
  } else if (target_type == "template") {
    # Replace local/config indicators with template
    new_base <- base_name
    if (grepl("Local", new_base)) {
      new_base <- gsub("Local", "Template", new_base)
    } else if (grepl("LOCAL", new_base)) {
      new_base <- gsub("LOCAL", "TEMPLATE", new_base)
    } else if (grepl("Config", new_base)) {
      new_base <- gsub("Config", "Template", new_base)
    } else if (grepl("CONFIG", new_base)) {
      new_base <- gsub("CONFIG", "TEMPLATE", new_base)
    } else if (grepl("local", new_base, ignore.case = TRUE)) {
      new_base <- gsub("local", "template", new_base, ignore.case = TRUE)
    } else if (grepl("config", new_base, ignore.case = TRUE)) {
      new_base <- gsub("config", "template", new_base, ignore.case = TRUE)
    } else {
      # No indicator found, append _template
      new_base <- paste0(base_name, "_template")
    }
  }
  
  return(paste0(new_base, ".", extension))
}

#' Confirm Fuzzy Match
#'
#' Asks user to confirm using a fuzzy-matched file.
#'
#' @param original_input Character string with the user's original input
#' @param fuzzy_match Character string with the fuzzy-matched filename
#' @param file_type Character string, "template" or "local"
#' @return Logical TRUE if user confirms, FALSE otherwise
#' @keywords internal
.confirm_fuzzy_match <- function(original_input, fuzzy_match, file_type) {
  
  fuzzy_basename <- basename(fuzzy_match)
  
  .icy_alert(paste0("No exact match for '", original_input, "'. Found '", fuzzy_basename, "'. Use this instead?"))
  
  # Interactive confirmation prompt
  confirm_prompt <- "Continue with fuzzy match? (Y/n): "
  user_input <- readline(confirm_prompt)
  
  if (tolower(trimws(user_input)) %in% c("", "y", "yes")) {
    .icy_success(paste0("Using '", fuzzy_match, "'"))
    return(TRUE)
  } else {
    .icy_inform(paste0("Fuzzy match declined for ", file_type, " file"))
    return(FALSE)
  }
}