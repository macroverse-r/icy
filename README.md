# icy

[![R-CMD-check](https://github.com/macroverse-r/icy/workflows/R-CMD-check/badge.svg)](https://github.com/macroverse-r/icy/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/icy)](https://CRAN.R-project.org/package=icy)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)

## Overview

`icy` (Interface for Configuration using YAML) is an R package that provides a comprehensive interface for managing configuration using YAML files. It enables R packages to handle settings and user preferences through structured YAML configuration files, supporting both interactive setup workflows and programmatic access patterns.

### Key Features

- Single source of truth: Local config file is the only authoritative source for configuration values
- Interactive configuration: User-friendly prompts with template integration for easy setup
- Type-aware YAML writing: Automatic type conversion based on template specifications
- Conflict detection: Detects and resolves conflicts when session environment variables shadow local config
- Flexible file discovery: Support for different YAML file naming conventions (snake_case, camelCase, PascalCase, kebab-case)
- Validation: Validate variable names against package templates
- User-friendly utilities: Toggle debug/verbose modes, display configurations, and more
- Smart file pairing: Intelligent auto-detection and fuzzy matching of template/local file pairs with user confirmation

## Installation

Install the development version from GitHub:

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install from GitHub
devtools::install_github("macroverse-r/icy")
```

## Core Functions

### Setup and Creation
| Function | Description |
|----------|-------------|
| `create_local()` | Create local configuration file from template. Essential for `.onLoad()` - copies template to user's local config directory |
| `create_template()` | Create a package template YAML file in `inst/` |
| `update_template()` | Update an existing template with interactive builder |

### Configuration Reading
| Function | Description |
|----------|-------------|
| `get_config()` | Return configuration as a named list. Reads from local config (default) or template (for inspection) |
| `get_package_name()` | Automatically detect calling package name using stack inspection |

### Configuration Writing
| Function | Description |
|----------|-------------|
| `write_local()` | Write/update variables in local YAML configuration. Preserves structure, validates against template |
| `qconfig()` | Interactive single-variable configuration. Template integration, type detection, path validation |
| `setup()` | Interactive multi-variable setup wizard. Progress tracking, skip logic, batch operations |

### Conflict Resolution
| Function | Description |
|----------|-------------|
| `check_conflicts()` | Detect and resolve conflicts between local config and session environment variables. Checks .Renviron for conflict origin |

### Utilities and Display
| Function | Description |
|----------|-------------|
| `show_config()` | Display local config values with template-diff indicators showing where values differ from defaults |
| `validate()` | Validate variable names against package template |
| `validate_config_file()` | Validate YAML file structure |
| `toggle_debug()` / `toggle_verbose()` | Toggle package debug/verbose modes |
| `edit_local()` | Open local config file in your preferred editor |
| `clean_dir_path()` | Path cleaning utility |

## Configuration System Overview

### Architecture

```
Template (inst/package_config_template.yml) [Blueprint - read-only]
    |
    | (used to create initial local config via create_local())
    v
Local Config (~/.local/share/R/package/) [SINGLE SOURCE OF TRUTH]
    |
    | get_config() reads from here
    | check_conflicts() ensures no ambiguity with session env
    v
R Session (conflicts detected and resolved, not used as config source)
```

The local config file is the only authoritative source for configuration values. Templates serve as blueprints for creating local configs and for validation. Session environment variables are not used as a configuration source -- if they conflict with local config values, `check_conflicts()` helps resolve the ambiguity.

### Workflow Summary

1. Template Creation: Define all possible configuration variables in `inst/package_config_template.yml`
2. Local Config: `create_local()` copies template values to the user's local config directory
3. Reading: `get_config()` reads from the local config file
4. User Interface: Provide configuration functions using `qconfig()`, `setup()`, or custom wrappers

<details>
<summary><strong>Quick Start</strong></summary>

```r
# 1. Create your template (see Step 1 below for details)
# inst/mypackage_config_template.yml

# 2. In your R/zzz.R:
.onLoad <- function(libname, pkgname) {
  icy::create_local()  # Creates user's local config from template
}

# 3. In your package functions - access config:
get_api_key <- function() {
  config <- icy::get_config()
  return(config$MYPACKAGE_API_KEY)
}

# 4. Expose configuration to users:
configure_mypackage <- function() {
  icy::setup()  # Interactive configuration for all variables
}
```

</details>

<details>
<summary><strong>Step 1: Create Your Package Template</strong></summary>

Define all possible configuration variables for your package. Create a template YAML file in your package's `inst/` directory (e.g., `inst/dummy_config_template.yml`):

```yaml
# Template configuration for dummy package
# This is the "blueprint" - defines ALL variables your package might use

default:
  DUMMY_API_KEY: "your-api-key-here"  # User will replace this
  DUMMY_DB_HOST: "localhost"          # Safe default
  DUMMY_DB_PORT: 5432                 # Default PostgreSQL port
  DUMMY_DATA_DIR: "~/dummy_data"      # User's home directory
  DUMMY_LOG_LEVEL: "INFO"             # Moderate logging
  DUMMY_VERBOSE: TRUE                 # Help users understand what's happening
  DUMMY_DEBUG: FALSE                  # Debug off by default
  DUMMY_TIMEOUT: 30                   # 30 seconds timeout

# Advanced: Different environments
production:
  DUMMY_DB_HOST: "prod.database.com"
  DUMMY_LOG_LEVEL: "WARNING"          # Less verbose in production
  DUMMY_VERBOSE: FALSE
  DUMMY_DEBUG: FALSE

development:
  DUMMY_LOG_LEVEL: "DEBUG"            # More details for developers
  DUMMY_VERBOSE: TRUE
  DUMMY_DEBUG: TRUE
```

The template serves as documentation for users AND validation for your package.

</details>

<details>
<summary><strong>Step 2: Integrate With Your Package</strong></summary>

```r
# In your R/zzz.R file
.onLoad <- function(libname, pkgname) {
  # Creates ~/.local/share/R/dummy/dummy_config_local.yml if it doesn't exist
  icy::create_local()
}

# In your package functions - read from local config
get_my_api_key <- function() {
  config <- icy::get_config()
  return(config$DUMMY_API_KEY)
}

# Interactive configuration for users
configure_package <- function() {
  icy::qconfig("DUMMY_API_KEY")      # Interactive with template integration
  icy::qconfig("DUMMY_VERBOSE")      # Automatic boolean detection (TRUE/FALSE options)
}
```

Local configs are always stored in the user's config directory (`~/.local/share/R/package/` on Linux, platform-appropriate directories on macOS/Windows via `tools::R_user_dir()`).

</details>

<details>
<summary><strong>Step 3: Understanding What Happens for Users</strong></summary>

#### First Time User Experience
1. User installs and loads your package
2. `icy::create_local()` copies template to their local config directory
3. User can now customize settings without affecting other users or projects

#### Ongoing Usage
```r
# User can see current settings (with template-diff indicators)
icy::show_config(package = "dummy")

# User can modify local settings (programmatic)
icy::write_local(
  package = "dummy",
  var_list = list(DUMMY_API_KEY = "my-real-key")
)

# Or use interactive configuration (user-friendly)
icy::qconfig("DUMMY_API_KEY", package = "dummy")    # Prompts with template options
icy::qconfig("DUMMY_VERBOSE", package = "dummy")    # Automatic TRUE/FALSE options

# Open config file directly in editor
icy::edit_local(package = "dummy")
```

</details>

## Usage Patterns

Click on any pattern below to expand and learn more about that use case:

<details>
<summary><strong>Pattern 1: "I want to see what's currently configured"</strong></summary>

Understanding your configuration state:

```r
library(icy)

# See all variables and their current values
# Values that differ from template defaults are highlighted
show_config(package = "dummy")

# Read config programmatically
config <- get_config(package = "dummy")
print(config$DUMMY_API_KEY)

# Inspect template defaults for comparison
template_defaults <- get_config(package = "dummy", origin = "template")
```

</details>

<details>
<summary><strong>Pattern 2: "My users need a smooth first-time setup experience"</strong></summary>

As a package developer, provide an optimal first-time user experience:

```r
# Your package's .onLoad() (in R/zzz.R)
.onLoad <- function(libname, pkgname) {
  icy::create_local()  # Creates local config from template automatically
}

# Provide a dedicated setup function for users
setup_dummy <- function() {
  message("Welcome to dummy package configuration!")
  icy::setup()  # Interactive setup walks through all variables
  message("\nConfiguration complete! Your settings are saved locally.")
}

# Individual configuration functions for specific needs
configure_api_key <- function() {
  icy::qconfig("DUMMY_API_KEY")  # Interactive with validation
}

configure_directories <- function() {
  icy::qconfig("DUMMY_DATA_DIR", type = "path")  # With directory creation
}

# Advanced: Targeted setup for critical variables
setup_essentials <- function() {
  icy::setup(
    vars = c("DUMMY_API_KEY", "DUMMY_DB_HOST"),
    allow_skip = c(FALSE, TRUE)  # API key required, DB host optional
  )
}
```

</details>

<details>
<summary><strong>Pattern 3: "I need different settings for different environments"</strong></summary>

Working with development, testing, and production:

```r
# Development settings (from template section)
dev_defaults <- get_config(
  package = "dummy",
  section = "development",
  origin = "template"
)

# User overrides in local config
write_local(
  var_list = list(
    DUMMY_DB_HOST = "my-local-dev-db.internal",
    DUMMY_DEBUG = TRUE
  ),
  package = "dummy"
)

# Read from a specific section of local config
prod_config <- get_config(package = "dummy", section = "production")
```

</details>

<details>
<summary><strong>Pattern 4: "I want to reset to template defaults"</strong></summary>

When you need to start over:

```r
# Get template defaults
template_defaults <- get_config(package = "dummy", origin = "template")

# Overwrite local config with template values
write_local(var_list = template_defaults, package = "dummy")
```

</details>

<details>
<summary><strong>Pattern 5: "There are conflicting environment variables in my session"</strong></summary>

When session environment variables (from .Renviron or `Sys.setenv()`) shadow your local config:

```r
# Detect and interactively resolve all conflicts
check_conflicts(package = "dummy")
# For each conflict, you choose: keep local config value or adopt session value
# Conflicting session variables are cleaned up
# .Renviron entries can be removed if that's the source

# get_config() also checks automatically (warns once per session)
config <- get_config(package = "dummy")
# If conflicts exist, you'll see a one-time warning suggesting check_conflicts()
```

`check_conflicts()` supports different modes:
- `mode = "resolve"` (default): Interactive resolution
- `mode = "warn"`: Print warnings without interactive prompts
- `mode = "silent"`: No output

</details>

<details>
<summary><strong>Pattern 6: "I'm debugging configuration issues"</strong></summary>

When things aren't working as expected:

```r
# Turn on verbose mode to see what icy is doing
toggle_verbose(package = "dummy")

# Turn on debug mode for even more details
toggle_debug(package = "dummy")

# Now icy functions will be much more chatty
config <- get_config(package = "dummy", verbose = TRUE)

# Validate that your variable names are correct
validate(
  package = "dummy",
  var_names = c("DUMMY_API_KEY", "DUMMY_TYPO_VAR")  # Will catch the typo
)

# Check for conflicts with session environment
check_conflicts(package = "dummy")
```

</details>

<details>
<summary><strong>Pattern 7: "I can't remember the exact config filename"</strong></summary>

icy provides intelligent fuzzy matching when you're unsure of exact filenames:

```r
# Partial filename matching
icy::qconfig("API_KEY", fn_tmpl = "dummyRunTemplat", package = "dummy")
# icy detects "dummyRunTemplate.yml" exists and asks:
# "No exact match for 'dummyRunTemplat'. Found 'dummyRunTemplate.yml'. Use this instead?"
# After confirmation, creates corresponding "dummyRunLocal.yml"

# Works for both template and local files
icy::setup(fn_local = "myConfigLocal", package = "dummy")
# Finds "myConfigTemplate.yml" and asks for confirmation
```

This prevents frustrating "file not found" errors when working with multiple config files or complex naming patterns.

</details>

<details>
<summary><strong>Pattern 8: "I want to expose clean configuration functions to my users"</strong></summary>

As a package developer, provide user-friendly configuration interfaces:

```r
# Create simple wrappers for common configuration tasks
set_api_key <- function(key = NULL) {
  if (is.null(key)) {
    icy::qconfig("MYPACKAGE_API_KEY")  # Interactive mode
  } else {
    icy::write_local(list(MYPACKAGE_API_KEY = key))  # Programmatic mode
    message("API key configured successfully")
  }
}

# Complete package setup function
setup_mypackage <- function(interactive = TRUE) {
  if (interactive) {
    message("Welcome to MyPackage configuration!")
    icy::setup()
  } else {
    icy::create_local()
  }
}

# Configuration viewing function
show_mypackage_config <- function() {
  icy::show_config()
  message("\nTo modify settings, run setup_mypackage() or set_api_key()")
}

# Validation function for critical settings
validate_mypackage_config <- function() {
  config <- icy::get_config()

  if (is.null(config$MYPACKAGE_API_KEY) || config$MYPACKAGE_API_KEY == "your-api-key-here") {
    stop("API key not configured. Run set_api_key() to configure.")
  }

  return(config)
}
```

This pattern gives users simple, package-specific functions while leveraging icy's power behind the scenes.

</details>

## Technical Reference

<details>
<summary><strong>File Naming Conventions</strong></summary>

Using the `case_format` argument, several functions support multiple naming conventions for configuration files:

- snake_case (default): `package_config_local.yml`
- camelCase: `packageConfigLocal.yml`
- PascalCase: `PackageConfigLocal.yml`
- kebab-case: `package-config-local.yml`

Alternatively, you can use custom names via `fn_local` and `fn_tmpl` parameters.

</details>

<details>
<summary><strong>Sync Parameter in write_local()</strong></summary>

`write_local()` supports an optional `sync` parameter as a convenience for writing through to the session environment. This does not make the session a configuration source -- it's purely for convenience when packages use `Sys.getenv()` internally.

```r
# Write to local config only (default)
write_local(var_list = list(DUMMY_TIMEOUT = 60), package = "dummy")

# Also set in session for immediate effect
write_local(var_list = list(DUMMY_TIMEOUT = 60), package = "dummy", sync = "all")
```

Sync options:
- `sync = "conservative"` (default): Only sync variables already in session
- `sync = "all"`: Sync all written variables to session
- `sync = "none"`: Skip session synchronization
- `sync = c("VAR1", "VAR2")`: Sync only specified variables

</details>

## License

This package is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0).
