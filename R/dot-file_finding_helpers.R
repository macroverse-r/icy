#' File Finding Helper Functions
#'
#' Internal functions supporting .find_config_files with deterministic
#' name-based file resolution.
#'
#' @name file-finding-helpers
#' @keywords internal
NULL

#' Resolve Name to Template Filename
#'
#' Converts the name parameter into a template filename string.
#' Handles three input forms: full path, full filename, or keyword.
#'
#' @param package Character string with package name
#' @param name Optional character string: keyword, filename, or path
#' @return Character string with template filename (basename only)
#' @keywords internal
.resolve_template_name <- function(package, name) {
  if (is.null(name)) return(.template_filename(package))

  # Full path: extract basename
  if (grepl("[/\\\\]", name)) {
    name <- basename(name)
  }

  # Ensure .yml extension
  if (!grepl("\\.(ya?ml)$", name, ignore.case = TRUE)) {
    name <- paste0(name, ".yml")
  }

  # Full filename (starts with {package}_ and has _config or _template suffix)
  base <- tools::file_path_sans_ext(name)
  if (startsWith(base, paste0(package, "_")) && grepl("_(config|template)$", base)) {
    if (grepl("_config$", base)) return(.swap_filename(name))
    return(name)
  }

  # Keyword: generate template filename
  .template_filename(package, tools::file_path_sans_ext(name))
}

#' Search for a Single File
#'
#' Searches for a file using exact matching first, then fuzzy matching if enabled.
#'
#' @param filename Character string with file path/name to search
#' @param package Character string with package name
#' @param type Character string: "template" or "config"
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

  # For templates in package dev mode, check inst/ directory directly
  is_dev_pkg <- type == "template" && .is_pkg_dir(package)
  if (is_dev_pkg) {
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

  # Get search directory (skip .is_pkg_dir() re-check for dev packages)
  package_dir <- if (is_dev_pkg) file.path(getwd(), "inst") else .get_config_dir(package = package, type = type)

  # Fast path: direct file.exists() before list.files()
  # Skip for dev packages -- already checked in the inst/ loop above
  if (!is_dev_pkg) {
    direct_path <- file.path(package_dir, filename)
    if (file.exists(direct_path) && !dir.exists(direct_path)) {
      return(list(path = normalizePath(direct_path, winslash = "/"), fuzzy = FALSE))
    }

    # Try adding .yml/.yaml extensions if not already present
    if (!grepl("\\.(ya?ml)$", filename, ignore.case = TRUE)) {
      for (ext in c(".yml", ".yaml")) {
        candidate <- file.path(package_dir, paste0(filename, ext))
        if (file.exists(candidate) && !dir.exists(candidate)) {
          return(list(path = normalizePath(candidate, winslash = "/"), fuzzy = FALSE))
        }
      }
    }
  }

  # Search subdirectories for exact basename match
  yaml_files <- list.files(
    path = package_dir,
    pattern = "\\.ya?ml$",
    recursive = TRUE,
    full.names = TRUE
  )

  # Filter out directories
  yaml_files <- yaml_files[!dir.exists(yaml_files)]

  # For template searches, exclude default_config subdirectories
  if (type == "template") {
    yaml_files <- yaml_files[!grepl("/default_config/", yaml_files)]
  }

  # Check for exact basename match (file may be in a subdirectory)
  exact_matches <- yaml_files[basename(yaml_files) == basename(filename)]
  if (length(exact_matches) > 0) {
    if (verbose && !grepl("[/\\\\]", filename)) {
      .icy_text(paste0("Found ", basename(filename), " in: ", dirname(exact_matches[1])))
    }
    return(list(path = exact_matches[1], fuzzy = FALSE))
  }

  # Fuzzy matching is only available in interactive sessions
  if (!fuzzy || !interactive()) {
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
  generic_terms <- c("template", "tmpl", "config", "cfg", "conf")
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

#' Confirm Fuzzy Match
#'
#' Asks user to confirm using a fuzzy-matched file.
#'
#' @param original_input Character string with the user's original input
#' @param fuzzy_match Character string with the fuzzy-matched filename
#' @param file_type Character string, "template" or "config"
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
