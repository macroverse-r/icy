library(yml2renv)

# Test the get_package_name function
cat("Testing get_package_name()...\n")
pkg_name <- get_package_name()
cat("Package name detected:", pkg_name, "\n")

# Define a function that calls get_package_name from another context
test_in_function <- function() {
  pkg_name <- get_package_name()
  cat("Package name from inside function:", pkg_name, "\n")
}

# Test the function
test_in_function()