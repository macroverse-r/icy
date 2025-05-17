# yml2renv

[![R-CMD-check](https://github.com/benjaminpeeters/yml2renv/workflows/R-CMD-check/badge.svg)](https://github.com/benjaminpeeters/yml2renv/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/yml2renv)](https://CRAN.R-project.org/package=yml2renv)

## Overview

`yml2renv` is an R package designed to manage environment variables in `.Renviron` files using YAML configuration. This package makes it easier for other R packages to configure and rely on environment variables in a consistent way.

### Key Features

- Load environment variable definitions from YAML configuration files
- Write and update variables in the user's `.Renviron` file
- Sync environment variables between the `.Renviron` file and the current R session
- Validate environment variable names against a predefined list
- Support for different YAML file naming conventions (snake_case, camelCase, etc.)

## Installation

### From GitHub (SSH)

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install the package from GitHub using SSH
devtools::install_git("git@github.com:benjaminpeeters/yml2renv.git")
```

### From GitHub (HTTPS)

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install the package from GitHub using HTTPS
devtools::install_github("benjaminpeeters/yml2renv")
```

## Usage

### Basic Example

```r
library(yml2renv)

# Get all environment variable names defined in your package's YAML config
var_names <- get_env_var_names()

# Display current values of environment variables
display_env_var_values()

# Write variables to .Renviron
write_vars_to_renviron(list(
  MY_API_KEY = "secret-key-value",
  MY_DATA_DIR = "/path/to/data"
))

# Sync variables from .Renviron to current R session
sync_env_vars()
```

### Using in Your Package

1. Create a YAML configuration file (e.g., `inst/env_vars.yml`) with your environment variables:

```yaml
environment_variables:
  - PACKAGE_DATA_DIR
  - PACKAGE_API_KEY
  - PACKAGE_DEBUG_MODE
```

2. Use the yml2renv functions in your package to manage these variables:

```r
#' Configure Your Package
#' 
#' @export
configure_my_package <- function() {
  # Get environment variable names
  vars <- yml2renv::get_env_var_names(package = "mypackage")
  
  # Prompt user for values and write to .Renviron
  # (Implementation depends on your package's needs)
  
  # Sync variables to make them available in the current session
  yml2renv::sync_env_vars(vars)
}
```

## License

This package is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0).