#' Calculate String Similarity for Fuzzy Matching
#'
#' Uses dual scoring approach to find similar filenames: full string similarity
#' and content-based similarity after removing generic terms and package name.
#' @param pattern The pattern to search for (user input)
#' @param candidate A candidate filename to compare against
#' @param package Package name to remove from comparison (optional)
#' @param debug Logical. If TRUE, displays detailed similarity calculations. Defaults to FALSE.
#' @return Numeric similarity score between 0 and 1
#' @keywords internal
.calculate_filename_similarity <- function(pattern, candidate, package = NULL, debug = FALSE) {
  # Work with basenames only, remove paths
  pattern_base <- basename(pattern)
  candidate_base <- basename(candidate)
  
  # Remove extensions for comparison
  pattern_no_ext <- sub("\\.(ya?ml)$", "", pattern_base, ignore.case = TRUE)
  candidate_no_ext <- sub("\\.(ya?ml)$", "", candidate_base, ignore.case = TRUE)
  
  # Case-insensitive comparison
  pattern_lower <- tolower(pattern_no_ext)
  candidate_lower <- tolower(candidate_no_ext)
  
  # Perfect match (ignoring case and extension)
  if (pattern_lower == candidate_lower) {
    return(1.0)
  }
  
  # Calculate FIRST similarity (full strings with edit distance)
  edit_dist_full <- utils::adist(pattern_lower, candidate_lower)[1, 1]
  max_len_full <- max(nchar(pattern_lower), nchar(candidate_lower))
  similarity_full <- if (max_len_full > 0) 1 - (edit_dist_full / max_len_full) else 0
  
  # Generic terms to remove for SECOND similarity
  generic_terms <- c("template", "tmpl", "local", "config", "cfg", "conf")
  if (!is.null(package) && nchar(package) > 0) {
    generic_terms <- c(generic_terms, tolower(package))
  }
  
  # Function to remove generic terms
  remove_generic <- function(str) {
    # Split by common delimiters
    parts <- unlist(strsplit(str, "[_.-]"))
    # Filter out generic terms (case-insensitive)
    parts_filtered <- parts[!tolower(parts) %in% generic_terms]
    # Rejoin with underscore
    if (length(parts_filtered) > 0) {
      paste(parts_filtered, collapse = "_")
    } else {
      # If everything was generic, return empty string
      ""
    }
  }
  
  pattern_stripped <- remove_generic(pattern_lower)
  candidate_stripped <- remove_generic(candidate_lower)
  
  # Calculate SECOND similarity (without generic terms)
  if (pattern_stripped == "" && candidate_stripped == "") {
    # Both are purely generic - use full similarity
    similarity_stripped <- similarity_full
  } else if (pattern_stripped == "" || candidate_stripped == "") {
    # One has no meaningful content after stripping - very different!
    similarity_stripped <- 0
  } else if (pattern_stripped == candidate_stripped) {
    similarity_stripped <- 1.0
  } else {
    edit_dist_stripped <- utils::adist(pattern_stripped, candidate_stripped)[1, 1]
    max_len_stripped <- max(nchar(pattern_stripped), nchar(candidate_stripped))
    similarity_stripped <- if (max_len_stripped > 0) 1 - (edit_dist_stripped / max_len_stripped) else 0
  }
  
  # Debug output if requested
  if (debug) {
    .icy_text(paste0("Similarity for '", pattern_base, "' vs '", candidate_base, "':"))
    .icy_bullets(c(
      paste0("Full similarity: ", round(similarity_full, 3)),
      paste0("Stripped similarity: ", round(similarity_stripped, 3)),
      paste0("Pattern stripped: '", pattern_stripped, "'"),
      paste0("Candidate stripped: '", candidate_stripped, "'"),
      paste0("Final score: ", round((similarity_full + similarity_stripped) / 2, 3))
    ), bullet = "none")
  }
  
  # Return mean of both similarities
  return((similarity_full + similarity_stripped) / 2)
}

#' Search for YAML Files Matching Pattern in Package Directory
#'
#' Performs recursive search for YAML configuration files matching a specified
#' filename pattern within a package's directory structure. This function is the
#' core file discovery mechanism used by configuration loading functions.
#'
#' The search algorithm:
#' 1. Locates the package directory using `get_package_path()`
#' 2. Recursively finds all YAML files (*.yml, *.yaml) in the directory tree
#' 3. Filters results using case-insensitive pattern matching
#' 4. Returns full file paths for matched files
#' 5. Optionally warns about multiple matches or no matches
#'
#' @param package Character string with the package name. 
#'   Defaults to `get_package_name()` to detect the calling package.
#' @param fn_pattern Character string with the filename pattern to match against YAML files.
#'   Uses regex pattern matching (case-insensitive).
#' @param type Character string specifying the type of configuration files to search for.
#'   Options: "local" (user-specific configs, default) or "template" (package-provided configs).
#' @param verbose Logical. If TRUE, displays informative messages about the search process
#'   and warnings for multiple/no matches. Defaults to FALSE.
#' @param user_dir Deprecated. Use `type` parameter instead. Logical for backwards compatibility.
#'
#' @return Character vector of matching file paths. Empty vector if no matches found.
#'
#' @keywords internal
.find_matching_pattern <- function(package = get_package_name(),
                                   fn_pattern,
                                   type = "local",
                                   verbose = FALSE,
                                   user_dir = NULL) {
  # Handle deprecated user_dir parameter
  if (!is.null(user_dir)) {
    .icy_warn("Parameter 'user_dir' is deprecated. Use 'type' parameter instead.")
    type <- if (user_dir) "local" else "template"
  }
  
  # Validate type parameter
  if (!type %in% c("local", "template")) {
    .icy_stop("Parameter 'type' must be either 'local' or 'template'")
  }
  
  # Get the package path
  tryCatch(
    {
      package_dir <- get_package_path(
        package = package,
        type = type
      )
    },
    error = function(e) {
      .icy_stop(paste0("Error locating package path: ", e$message))
    }
  )
  if (verbose) .icy_text(paste0("package_dir = ", package_dir))

  # Use list.files to recursively find yaml files
  yaml_files <- list.files(
    path = package_dir,
    pattern = "\\.ya?ml$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Filter out any directories (only keep actual files)
  yaml_files <- yaml_files[!dir.exists(yaml_files)]
  
  # For template searches, exclude files in local_config subdirectories
  if (type == "template") {
    yaml_files <- yaml_files[!grepl("/local_config/", yaml_files)]
  }

  # Calculate similarity scores for all files
  similarities <- sapply(yaml_files, function(f) {
    .calculate_filename_similarity(fn_pattern, f, package = package)
  })
  
  # Calculate discrimination ratios and apply boost
  boosted_similarities <- similarities
  
  if (length(similarities) > 1) {
    # For each file, calculate how much better it matches than others
    for (i in seq_along(yaml_files)) {
      this_score <- similarities[i]
      
      if (this_score > 0) {
        # Get all other scores
        other_scores <- similarities[-i]
        
        # Find the next best score and average
        if (length(other_scores) > 0) {
          next_best <- max(other_scores)
          avg_others <- mean(other_scores[other_scores > 0])
          
          # Two-factor boost strategy:
          # 1. Discrimination from next best (how unique is this match?)
          # 2. Discrimination from average (how much better than typical?)
          
          if (next_best > 0) {
            # Calculate discrimination ratio
            discrimination_ratio <- this_score / next_best
            
            # Apply boost if this file discriminates well
            if (discrimination_ratio > 1) {
              # More aggressive boosting formula
              # Uses both sqrt(ratio) and a multiplier based on how much better than average
              boost_factor <- sqrt(discrimination_ratio)
              
              # Additional boost if much better than average
              if (!is.na(avg_others) && avg_others > 0) {
                avg_ratio <- this_score / avg_others
                # If 2x better than average, multiply boost by 1.5
                # If 3x better, multiply by 2, etc.
                avg_boost <- 1 + (avg_ratio - 1) * 0.5
                boost_factor <- boost_factor * min(avg_boost, 2.5)  # Cap at 2.5x
              }
              
              boosted_similarities[i] <- min(1, similarities[i] * boost_factor)
            }
          }
        }
      }
    }
  }
  
  # Lower threshold to 0.4 for better fuzzy matching with boost
  # The boost will help discriminate between good and bad matches
  good_matches <- yaml_files[boosted_similarities >= 0.4]
  
  if (length(good_matches) > 0) {
    # Sort by boosted similarity score (best matches first)
    good_matches <- good_matches[order(boosted_similarities[boosted_similarities >= 0.4], decreasing = TRUE)]
  }

  if (verbose) {
    if (length(good_matches) == 0) {
      .icy_warn(paste0("No YAML file in ", package_dir, " matching ", fn_pattern))
    } else if (length(good_matches) > 1) {
      .icy_warn(
        paste0("Multiple config YAML files found: ", paste(basename(good_matches), collapse = ", "), ". Using best match.")
      )
    }
  }

  return(as.character(good_matches))
}
