#!/usr/bin/env Rscript

# Script to update yml2renv package documentation and install locally
# Run with: Rscript update_package.R

message("Updating yml2renv package...")

# Step 1: Update documentation
cat("\nUpdating documentation...\n")
devtools::document()

# Step 2: Increment version in DESCRIPTION (patch level)
cat("\nIncrementing version...\n")
desc_file <- "DESCRIPTION"
desc_content <- readLines(desc_file)
version_line <- grep("^Version:", desc_content, value = TRUE)
current_version <- gsub("^Version:\\s*", "", version_line)
version_parts <- strsplit(current_version, "\\.")[[1]]
version_parts[length(version_parts)] <- as.character(as.integer(version_parts[length(version_parts)]) + 1)
new_version <- paste(version_parts, collapse = ".")
desc_content[grep("^Version:", desc_content)] <- paste0("Version: ", new_version)
writeLines(desc_content, desc_file)
cat("Version updated from", current_version, "to", new_version, "\n")

# Step 3: Install the package locally
cat("\nInstalling package locally...\n")
devtools::install(".", quiet = TRUE)

# Step 4: Run some basic checks on the installed package
cat("\nRunning basic checks...\n")
cat("Testing if get_package_name() is exported...\n")
get_package_name_exists <- "get_package_name" %in% ls("package:yml2renv")
cat("get_package_name exported:", get_package_name_exists, "\n")

# Print success message
cat("\nPackage yml2renv version", new_version, "has been successfully updated and installed!\n")
cat("You can now test it in other packages.\n")
