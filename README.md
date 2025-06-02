# icy

[![R-CMD-check](https://github.com/benjaminpeeters/icy/workflows/R-CMD-check/badge.svg)](https://github.com/benjaminpeeters/icy/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/icy)](https://CRAN.R-project.org/package=icy)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)

## Overview

`icy` (Interface for Configuration using YAML) is an R package that provides an interface for managing configuration in R packages using YAML files. This package makes it easier for R packages to configure and manage environment variables, settings, and preferences through YAML configuration files, supporting both interactive setup and automated workflows.

### Key Features

- Load environment variable definitions from YAML configuration files
- Write and update variables in the user's `.Renviron` file
- Sync environment variables between the `.Renviron` file and the current R session
- Validate environment variable names against a predefined list
- Support for different YAML file naming conventions (snake_case, camelCase, etc.)

## Main Functions

| Function | Description |
|----------|-------------|
| `get_env_var_names()` | Extract environment variable names from a package's YAML configuration |
| `get_config_path()` | Find a package's environment variables YAML configuration file |
| `write_to_renviron()` | Write environment variables to the .Renviron file |
| `erase_from_renviron()` | Remove environment variables from the .Renviron file |
| `sync_env_vars()` | Update current R session with variables from .Renviron |
| `display_env_vars()` | Display values of specified environment variables |
| `validate_env_var_names()` | Check if variable names are defined in package's configuration |
| `get_renviron_path()` | Get path to user's .Renviron file |
| `get_package_name()` | Get the name of the current package |
| `get_package_dir()` | Get the directory of a package |
| `clean_dir_path()` | Clean and normalize directory paths |

## Installation

Be sure that devtools is installed in your system:
```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
```

### From GitHub

Install the package from GitHub using SSH
```r
devtools::install_git("git@github.com:benjaminpeeters/icy.git")
```

Install the package from GitHub using HTTPS:
```r
devtools::install_github("benjaminpeeters/icy")
```

## Usage

### Basic Example

```r
library(icy)

# Get all environment variable names defined in your package's YAML config
var_names <- get_env_var_names(package = "mypackage")

# Display current values of environment variables
display_env_vars(var_names)

# Write variables to .Renviron
write_to_renviron(list(
  MY_API_KEY = "secret-key-value",
  MY_DATA_DIR = "/path/to/data"
))

# Sync variables from .Renviron to current R session
sync_env_vars(var_names)
```

### Using in Your Package

1. Create a YAML configuration file (e.g., `inst/env_vars.yml`) with your environment variables:

```yaml
environment_variables:
  - PACKAGE_DATA_DIR
  - PACKAGE_API_KEY
  - PACKAGE_DEBUG_MODE
```

2. Use the icy functions in your package to manage these variables:

```r
#' Configure Your Package
#' 
#' @export
configure_my_package <- function() {
  # Get environment variable names
  vars <- icy::get_env_var_names(package = "mypackage")
  
  # Prompt user for values and write to .Renviron
  # (Implementation depends on your package's needs)
  
  # Sync variables to make them available in the current session
  icy::sync_env_vars(vars)
}
```

## License

This package is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0).
