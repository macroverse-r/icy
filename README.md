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

The `icy` package is intended to work with:

- a template YAML file or .Renviron (the first is generally recommanded to avoid polluting the user .Renviron nor all their sessions / avoid conflict with other packages, etc.)
- `.onLoad()` with `create_local()` (if you want to work directly with the YAML file) or `load_config()` (if you want to use the R session environment variables)

For simplicity, we use the fake package called "dummy". Replace the names accordingly to fit your package name or desire.

### Using with template YAML file without R session env. variables

1. Create a YAML configuration file (e.g., `inst/mypackage_template_config.yml`) with your environment variables:

Create a YAML file called `mypackage_template_config.yml` (replace "mypackage" appropriately). For example, for the package "dummy", we could have `dummy_template_config.yml` (note: it does not have to be called this way: look at the options of `create_local()` for alternative ways):

```yaml
# Template configuration for dummy package
# This file defines all environment variables used by the dummy package

default:
  DUMMY_API_KEY: 1234
  DUMMY_DB_HOST: "localhost"
  DUMMY_DB_PORT: 5432
  DUMMY_DATA_DIR: "~/dummy_data"
  DUMMY_LOG_LEVEL: "INFO"
  DUMMY_VERBOSE: TRUE
  DUMMY_DEBUG: FALSE
  DUMMY_MAX_RETRIES: ~
  DUMMY_TIMEOUT: 30

production:
  DUMMY_API_KEY:
  DUMMY_DB_HOST: "prod.database.com"
  DUMMY_DB_PORT: 5432
  DUMMY_LOG_LEVEL: "WARNING"
  DUMMY_VERBOSE: FALSE
  DUMMY_DEBUG: FALSE

development:
  DUMMY_API_KEY: "dev-key-12345"
  DUMMY_DB_HOST: "localhost"
  DUMMY_DB_PORT: 5433
  DUMMY_LOG_LEVEL: "DEBUG"
  DUMMY_VERBOSE: TRUE
  DUMMY_DEBUG: TRUE
```

2. Use the icy functions in your package to automatically load or create a local config file:

Then, in the `R` folder in my `dummy repo`, I can create the following `zzz.R` file:

```R
#' Package startup
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Use icy to create local config in user's local if it does not exist already
  icy::create_local()
}
```

Note that we don't need to specify the package name because we are in the "dummy" pacakge here. If you want to automatically interact with other packages, then you will have to add the specific pacakge name to `icy`.

This will ensure that the local configuration folder of the user contains the file "dummy_template_config.yml" that will contain the configuration, is persistent between section, is user-specific, can be customizeable, and only be loaded when your package will be loaded by the user.

Note that the template file will never be overwritten by `icy`.
Note that only the default section of the template will be loaded in the local config file by default.

Without using the R session environment to store the variables, the user/developer can access the variables using `get_config()`. For example, this line store the value 5433 (as a integer) independently of the local configuration choice of the user (because we specify the origin as "template"):
```R
a <- icy::get_config(package = "dummy",
                     user = "development",
                     origin = "template")$DUMMY_DB_PORT
```



### Using with template YAML file without R session env. variables

If you like using the `Sys.setenv()` and `Sys.getenv()` or need your configuration variables to be available in your R session, you can write a templace config file as before and use the following .onLoad() function:

```R
#' Package startup
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Use icy to load my configuration
  icy::load_config(
    unset = list(DUMMY_API_KEY = 123,
                 DUMMY_MAX_RETRIES = 15)
  )
}
```

Now, in this case, I can use `Sys.getenv("DUMMY_API_KEY")`, which will be "1234" (as a string!), and `Sys.getenv("DUMMY_MAX_RETRIES")`, which will be "15" (as a string)

### Renviron

Imagine now, that we would have the following `.Renviron` file on the path:
```R
# I decided to defined my environment variables in my $HOME/.Renviron
DUMMY_DB_PORT: "Here is the .Renviron"
```

`Sys.getenv("DUMMY_MAX_RETRIES")` after `library(dummy)` will return "Here is the .Renviron" and not "5432" because the R session environment variables are not overwritten by default.

### Access and Modify Variables using local config


3. Use the icy functions in your package to manage these variables:

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


### Make user-friendly configuration function

Not yet implemented

### Additional Examples

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


## License

This package is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0).
