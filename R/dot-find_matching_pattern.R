#' Calculate String Similarity for Fuzzy Matching
#'
#' Uses multiple strategies to find similar filenames
#' @param pattern The pattern to search for (user input)
#' @param candidate A candidate filename to compare against
#' @return Numeric similarity score between 0 and 1
#' @keywords internal
.calculate_filename_similarity <- function(pattern, candidate) {
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
  
  # Calculate edit distance (typos, missing chars)
  edit_dist <- adist(pattern_lower, candidate_lower)[1,1]
  max_len <- max(nchar(pattern_lower), nchar(candidate_lower))
  
  # Normalize by length to get similarity
  if (max_len > 0) {
    similarity <- 1 - (edit_dist / max_len)
    
    # Boost score if one is a substring of the other
    if (grepl(pattern_lower, candidate_lower, fixed = TRUE) || 
        grepl(candidate_lower, pattern_lower, fixed = TRUE)) {
      similarity <- similarity * 1.2  # Boost but cap at 1
    }
    
    return(min(1, similarity))
  }
  
  return(0)
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
#' @param user_dir Logical indicating whether to search in user directory (TRUE, default)
#'   or package installation directory (FALSE).
#' @param verbose Logical. If TRUE, displays informative messages about the search process
#'   and warnings for multiple/no matches. Defaults to FALSE.
#'
#' @return Character vector of matching file paths. Empty vector if no matches found.
#'
#' @keywords internal
.find_matching_pattern <- function(package = get_package_name(),
                                   fn_pattern,
                                   user_dir = TRUE,
                                   verbose = FALSE) {
  # Get the package path
  tryCatch(
    {
      package_dir <- get_package_path(
        package = package,
        user_dir = user_dir
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

  # Calculate similarity scores for all files
  similarities <- sapply(yaml_files, function(f) {
    .calculate_filename_similarity(fn_pattern, f)
  })
  
  # Filter files with reasonable similarity (threshold of 0.5)
  good_matches <- yaml_files[similarities >= 0.5]
  
  if (length(good_matches) > 0) {
    # Sort by similarity score (best matches first)
    good_matches <- good_matches[order(similarities[similarities >= 0.5], decreasing = TRUE)]
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
