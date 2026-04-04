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

#' Search for a Single File in a Directory
#'
#' Pure file search: looks for a filename in a given directory using exact
#' matching first, then fuzzy matching if enabled. Does not resolve directories
#' -- the caller provides the search directory.
#'
#' @param filename Character string with filename to search for
#' @param search_dir Character string with directory to search in
#' @param fuzzy Logical. If TRUE, allows fuzzy matching
#' @param package Character string with package name (only for fuzzy scoring)
#' @param verbose Logical. If TRUE, shows detailed messages
#' @return List with:
#'   - path: Full path to file or NULL
#'   - fuzzy: TRUE if found via fuzzy matching, FALSE otherwise
#' @keywords internal
._search_file <- function(filename, search_dir, fuzzy = TRUE,
                          package = NULL, verbose = FALSE) {

  # Fast path: direct file.exists()
  direct_path <- file.path(search_dir, filename)
  if (file.exists(direct_path) && !dir.exists(direct_path)) {
    return(list(path = normalizePath(direct_path, winslash = "/"), fuzzy = FALSE))
  }

  # Recursive search for exact basename match
  yaml_files <- list.files(
    path = search_dir,
    pattern = "\\.ya?ml$",
    recursive = TRUE,
    full.names = TRUE
  )
  yaml_files <- yaml_files[!dir.exists(yaml_files)]

  exact_matches <- yaml_files[basename(yaml_files) == basename(filename)]
  if (length(exact_matches) > 0) {
    if (verbose) {
      .icy_text(paste0("Found ", basename(filename), " in: ", dirname(exact_matches[1])))
    }
    return(list(path = normalizePath(exact_matches[1], winslash = "/"), fuzzy = FALSE))
  }

  # Fuzzy matching (interactive sessions only)
  if (!fuzzy || !interactive()) {
    if (verbose) {
      .icy_warn(paste0("No exact match for '", filename, "' in ", search_dir))
    }
    return(list(path = NULL, fuzzy = FALSE))
  }

  similarities <- sapply(yaml_files, function(f) {
    ._calculate_filename_similarity(filename, f, package = package)
  })
  boosted_similarities <- ._boost_similarities(similarities, yaml_files)
  good_matches <- yaml_files[boosted_similarities >= 0.4]

  if (length(good_matches) > 0) {
    good_matches <- good_matches[order(boosted_similarities[boosted_similarities >= 0.4], decreasing = TRUE)]
    if (verbose) {
      .icy_alert(paste0("No exact match for '", filename, "'. Found fuzzy match: ", basename(good_matches[1])))
    }
    return(list(path = normalizePath(good_matches[1], winslash = "/"), fuzzy = TRUE))
  }

  if (verbose) {
    .icy_warn(paste0("No file matching '", filename, "' in ", search_dir))
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
