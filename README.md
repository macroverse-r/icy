# icy

[![R-CMD-check](https://github.com/macroverse-r/icy/workflows/R-CMD-check/badge.svg)](https://github.com/macroverse-r/icy/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/icy)](https://CRAN.R-project.org/package=icy)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)

## Overview

`icy` (Interface for Configuration using YAML) is an R package that provides a comprehensive interface for managing configuration using YAML files. It enables R packages to handle environment variables, settings, and user preferences through structured YAML configuration files, supporting both interactive setup workflows and programmatic access patterns.

### Key Features

- Multi-source configuration: Load from template, local config, or .Renviron files with priority resolution
- Interactive configuration: User-friendly prompts with template integration for easy setup
- Type-aware YAML writing: Automatic type conversion based on template specifications
- Flexible file discovery: Support for different YAML file naming conventions (snake_case, camelCase, PascalCase, kebab-case)
- Environment variable management: Write, update, and erase variables in .Renviron files
- Session synchronization: Sync environment variables between .Renviron and current R session
- Validation: Validate environment variable names against package templates
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

### Setup & Package Discovery
| Function | Description |
|----------|-------------|
| `create_local()` | Create local configuration file from template. Essential for `.onLoad()` - copies template section to user's local config directory |
| `load_config()` | Modify R session environment using `Sys.setenv()`. Sets environment variables directly in current session |
| `get_package_name()` | Automatically detect calling package name using stack inspection. Enables package-agnostic function calls |
| `get_package_path()` | Get package directory path. Context-aware: returns development path in package directories, installation path otherwise |
| `get_renviron_path()` | Get path to user's .Renviron file for global environment variable management |

### Configuration Management
| Function | Description |
|----------|-------------|
| `get_config()` | Return configuration as a list. Reads from specified origin without modifying session. Use for programmatic access |
| `find_local()` / `find_template()` | File discovery with fuzzy matching. Handles different case formats and naming conventions automatically |
| `write_local()` | Write/update variables in local YAML configuration. Preserves structure, validates against template, sync options |
| `write_renviron()` | Write/update variables in .Renviron file. Global settings with template validation and session sync |
| `erase_renviron()` | Remove variables from .Renviron file safely. Preserves file structure and other variables |
| `qconfig()` | Interactive single-variable configuration. Template integration, type detection, path validation, fuzzy matching |
| `setup()` | Interactive multi-variable configuration. Progress tracking, skip logic, batch operations with summary |

### Utilities & Display
| Function | Description |
|----------|-------------|
| `show_config()` | Configuration dashboard with multiple views: "values" (resolved with sources), "sources" (by origin), "full" (comprehensive). Essential for debugging |
| `validate()` | Template-based variable name validation. Prevents typos and ensures consistency with package expectations |
| `sync()` | Bidirectional environment variable synchronization. Critical for making config changes active in current session |
| `toggle_debug()` / `toggle_verbose()` | Developer utilities for debugging configuration issues. Writes to local config with immediate session sync |



## Configuration System Overview

The icy configuration workflow follows three main steps for package developers:

```
Step 1: Create Template → Step 2: Choose Strategy → Step 3: Expose to Users
     (Package Author)         (Package Integration)       (User Interface)
```

Workflow Summary:

1. Template Creation: Define all possible configuration variables in `inst/package_config_template.yml`
2. Integration Strategy: Choose between file-based config (`get_config()`) or environment variables (`load_config()`)
3. User Interface: Provide configuration functions using `qconfig()`, `setup()`, or custom wrappers

<details>
<summary><strong>Quick Start</strong></summary>

```r
# 1. Create your template (see Step 1 below for details)
# inst/mypackage_config_template.yml

# 2. In your R/zzz.R - choose your strategy:
.onLoad <- function(libname, pkgname) {
  icy::create_local()  # Creates user's local config from template
}

# 3. In your package functions - access config:
get_api_key <- function() {
  config <- icy::get_config(origin = "local")  # File-based approach
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
<summary><strong>Step 2: Choose Your Package Strategy</strong></summary>

You have two main approaches for using `icy` in your package:

#### Strategy A: Local Config Files (Recommended)
When to use: You want users to have persistent, customizable settings without polluting their global environment.

```r
# In your R/zzz.R file
.onLoad <- function(libname, pkgname) {
  # This creates ~/.local/share/R/dummy/dummy_config_local.yml if it doesn't exist
  icy::create_local()
  
  # Now users can edit their local config and it persists between sessions
}

# In your package functions
get_my_api_key <- function() {
  config <- icy::get_config(origin = "priority")  # Checks .Renviron > local
  return(config$DUMMY_API_KEY)
}

# Interactive configuration for users
configure_package <- function() {
  icy::qconfig("DUMMY_API_KEY")      # Interactive with template integration
  icy::qconfig("DUMMY_VERBOSE")      # Automatic boolean detection (TRUE/FALSE options)
}
```

Session synchronization: Both `write_local()` and `write_renviron()` support sync options (`"conservative"`, `"all"`, `"none"`, or specific variables). See Pattern 5 for detailed sync behavior and examples.

#### Strategy B: R Session Environment Variables
When to use: You want traditional `Sys.getenv()` / `Sys.setenv()` behavior.

```r
# In your R/zzz.R file
.onLoad <- function(libname, pkgname) {
  # Load config into R session environment variables
  icy::load_config(
    origin = "priority",  # .Renviron > local > template
    unset = list(         # Fallbacks for missing values
      DUMMY_API_KEY = "demo-key-12345",
      DUMMY_TIMEOUT = 30
    )
  )
}

# In your package functions  
get_my_api_key <- function() {
  return(Sys.getenv("DUMMY_API_KEY"))  # Standard R approach
}
```

Note: `load_config()` runs `create_local()`.

</details>

<details>
<summary><strong>Step 3: Understanding What Happens for Users</strong></summary>

#### First Time User Experience:
1. User installs and loads your package
2. `icy::create_local()` copies template to their local config directory
3. User can now customize settings without affecting other users or projects

#### Ongoing Usage:
```r
# User can see current settings
icy::show_config(package = "dummy")

# User can modify local settings (traditional approach)
icy::write_local(
  package = "dummy",
  var_list = list(DUMMY_API_KEY = "my-real-key")
)

# Or use interactive configuration (user-friendly)
icy::qconfig("DUMMY_API_KEY", package = "dummy")    # Prompts with template options
icy::qconfig("DUMMY_VERBOSE", package = "dummy")    # Automatic TRUE/FALSE options for boolean types

# User can set global settings (affects all projects)
icy::write_renviron(
  package = "dummy",
  var_list = list(DUMMY_API_KEY = "global-key")
)

# Changes take effect immediately in session environment
icy::sync(package = "dummy")
```

</details>

## Usage Patterns: Real-World Scenarios

Click on any pattern below to expand and learn more about that use case:

<details>
<summary><strong>Pattern 1: "I want to see what's currently configured"</strong></summary>

Understanding your configuration state is the first step:

```r
library(icy)

# See all variables and where they come from
show_config(package = "dummy")
# Output shows:
# DUMMY_API_KEY: "my-secret" (from: .Renviron)
# DUMMY_DB_HOST: "localhost" (from: local config)  
# DUMMY_DEBUG: FALSE (from: template)

# Check what's in each source individually
template_values <- get_config(package = "dummy", origin = "template")
local_values <- get_config(package = "dummy", origin = "local") 
renviron_values <- get_config(package = "dummy", origin = "renviron")

# See the final merged result (priority: .Renviron > local > template)
final_config <- get_config(package = "dummy", origin = "priority")
```

💡 Educational note: When developing, you can use `show_config()` frequently. It's your "dashboard" for understanding configuration state.

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
  
  # Use icy's interactive setup - walks through all variables
  icy::setup()
  
  message("\nConfiguration complete! Your settings are saved locally.")
  message("Run show_dummy_config() anytime to view your settings.")
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

# Users then have a smooth experience:
# library(dummy)           # Local config created automatically  
# setup_dummy()            # Interactive configuration for all variables
# setup_essentials()       # Just the critical ones
# configure_api_key()      # Individual settings as needed
```

💡 Educational note: Interactive functions like `setup()` and `qconfig()` provide template descriptions, validate inputs, and handle type conversion automatically.

🔧 Developer note: Local configs are saved in user-specific directories (~/.local/share/R/) for installed packages, or alongside templates for development packages.

</details>

<details>
<summary><strong>Pattern 3: "I want global settings across all my projects"</strong></summary>

Sometimes you want the same setting everywhere:

```r
# Set a global API key for all projects
icy::write_renviron(
  var_list = list(
    DUMMY_API_KEY = "my-global-key-for-all-projects"
  ),
  package = "dummy"  # Still validates against dummy's template
)

# This goes in ~/.Renviron and affects ALL R sessions
# Priority: .Renviron beats local config
```

💡 Educational note: .Renviron changes affect your entire R environment. Use sparingly!

</details>

<details>
<summary><strong>Pattern 4: "I need different settings for different environments"</strong></summary>

Working with development, testing, and production:

```r
# Development setup
dev_config <- get_config(
  package = "dummy",
  section = "development",  # Uses the "development" section from template
  origin = "template"
)

# User can override with local development settings
write_local(
  var_list = list(
    DUMMY_DB_HOST = "my-local-dev-db.internal",
    DUMMY_DEBUG = TRUE
  ),
  package = "dummy"
  # This goes in the "default" section of local config
)

# For production, user might set global .Renviron
write_renviron(
  var_list = list(
    DUMMY_DB_HOST = "prod-db.company.com",
    DUMMY_DEBUG = FALSE
  ),
  package = "dummy"
)
```

💡 Educational note: Template sections are read-only. User customizations always go in "default" section of local/renviron.

</details>

<details>
<summary><strong>Pattern 5: "I made changes and need them active now"</strong></summary>

Understanding the difference between file changes and session environment:

```r
# get_config() ALWAYS reads from files - never affected by session sync
file_config <- get_config(package = "dummy", origin = "local")
print(file_config$DUMMY_API_KEY)  # Reads current file value

# Sys.getenv() reads from R session environment
session_value <- Sys.getenv("DUMMY_API_KEY")  # May be different from file

# When you write_local(), the FILE is updated immediately
write_local(
  var_list = list(DUMMY_API_KEY = "new-key"),
  package = "dummy",
  sync = "none"  # Don't sync to session
)

# File updated, but session unchanged
print(get_config(package = "dummy")$DUMMY_API_KEY)  # "new-key" (from file)
print(Sys.getenv("DUMMY_API_KEY"))                  # Still old value (session)

# Sync options control SESSION environment updates:
write_local(
  var_list = list(DUMMY_VERBOSE = TRUE),
  package = "dummy",
  sync = "all"  # Update session immediately
)

# Now both file and session are updated
print(get_config(package = "dummy")$DUMMY_VERBOSE)  # TRUE (from file)
print(Sys.getenv("DUMMY_VERBOSE"))                  # "TRUE" (session)

# Manual session sync when needed
icy::sync(package = "dummy", var_names = "DUMMY_API_KEY")
print(Sys.getenv("DUMMY_API_KEY"))  # Now updated in session
```

Sync Options Summary:

- `sync = "conservative"` (default): Only sync variables already in session environment
- `sync = "all"`: Sync all written variables to session environment  
- `sync = "none"`: Skip session synchronization
- `sync = c("VAR1", "VAR2")`: Sync only specified variables

When syncing, the new written values are placed in the session environment with highest priority, giving immediate effect regardless of .Renviron values.

💡 Educational note: `get_config()` reads files, `Sys.getenv()` reads session. Sync controls whether changes propagate to session environment.

</details>

<details>
<summary><strong>Pattern 6: "I want to reset everything back to defaults"</strong></summary>

Sometimes you need to start over:

```r
# See what variables are configured
var_names <- names(get_config(package = "dummy"))

# Remove from .Renviron (global reset)
erase_renviron(var_names, package = "dummy")

# Could also reset local config by overwriting with template values
template_defaults <- get_config(package = "dummy", origin = "template")
write_local(var_list = template_defaults, package = "dummy")

# Apply changes to current session
sync(package = "dummy", var_names = var_names)
```

</details>

<details>
<summary><strong>Pattern 7: "I'm debugging configuration issues"</strong></summary>

When things aren't working as expected:

```r
# Turn on verbose mode to see what icy is doing
toggle_verbose(package = "dummy")

# Turn on debug mode for even more details
toggle_debug(package = "dummy") 

# Now icy functions will be much more chatty
config <- get_config(package = "dummy", origin = "priority", verbose = TRUE)

# Validate that your variable names are correct
validate(
  package = "dummy",
  var_names = c("DUMMY_API_KEY", "DUMMY_TYPO_VAR")  # Will catch the typo
)

# Check if files exist where expected
local_path <- find_local(package = "dummy")
template_path <- find_template(package = "dummy")
print(c(local = local_path, template = template_path))
```

Use `toggle_verbose()` and `toggle_debug()` when configuration isn't behaving as expected or during development phase.

</details>

<details>
<summary><strong>Pattern 8: I can't remember the exact config filename</strong> 🆕</summary>

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
<summary><strong>Pattern 9: I want to expose clean configuration functions to my users</strong> 🆕</summary>

As a package developer, provide user-friendly configuration interfaces:

```r
# Create simple wrappers for common configuration tasks
set_api_key <- function(key = NULL) {
  if (is.null(key)) {
    # Interactive mode
    icy::qconfig("MYPACKAGE_API_KEY")
  } else {
    # Programmatic mode
    icy::write_local(list(MYPACKAGE_API_KEY = key))
    message("API key configured successfully")
  }
}

# Complete package setup function
setup_mypackage <- function(interactive = TRUE) {
  if (interactive) {
    message("Welcome to MyPackage configuration!")
    icy::setup()  # Full interactive setup
  } else {
    # Non-interactive: just ensure local config exists
    icy::create_local()
  }
}

# Configuration viewing function
show_mypackage_config <- function() {
  icy::show_config(display = "values")
  message("\nTo modify settings, run setup_mypackage() or set_api_key()")
}

# Validation function for critical settings
validate_mypackage_config <- function() {
  config <- icy::get_config(origin = "priority")
  
  # Check required settings
  if (is.null(config$MYPACKAGE_API_KEY) || config$MYPACKAGE_API_KEY == "your-api-key-here") {
    stop("API key not configured. Run set_api_key() to configure.")
  }
  
  # Validate settings
  if (!is.logical(config$MYPACKAGE_VERBOSE)) {
    warning("MYPACKAGE_VERBOSE should be TRUE or FALSE")
  }
  
  return(config)
}

# Export these functions in your NAMESPACE for users:
# export(set_api_key, setup_mypackage, show_mypackage_config)
```

This pattern gives users simple, package-specific functions while leveraging icy's power behind the scenes.

</details>

## Technical Reference

<details>
<summary><strong>Priority Resolution System</strong></summary>

icy supports a four-layer priority system for advanced configuration scenarios:

```
🚀 Session Environment (Sys.getenv)     [Highest Priority]
           ↓ overrides
🌍 .Renviron (~/.Renviron)              [Global settings]
           ↓ overrides  
📝 Local Config (user customizable)     [Package-specific]
           ↓ fallback to
📋 Template (read-only defaults)        [Lowest Priority]
```

### Two Ways to Access Priority-Resolved Configuration:

```r
# Option 1: get_config() - Returns merged list (does NOT modify session)
config <- icy::get_config(origin = "priority")
api_key <- config$MY_PACKAGE_API_KEY  # Extract from returned list

# Getting a specific value (common patterns):
api_key <- icy::get_config(origin = "priority")$MY_PACKAGE_API_KEY  # Direct access
debug_mode <- icy::get_config(origin = "local")$MY_PACKAGE_DEBUG    # From local only

# With error checking for missing values:
config <- icy::get_config(origin = "priority")
if (is.null(config$MY_PACKAGE_API_KEY)) {
  stop("API key not configured. Run setup() to configure.")
}

# Option 2: load_config() - Sets environment variables (modifies session)
icy::load_config(origin = "priority")  # Uses Sys.setenv() internally
api_key <- Sys.getenv("MY_PACKAGE_API_KEY")  # Standard R environment access
```

### When to Use Priority Resolution:
- When users need global settings that apply across multiple R sessions (.Renviron)
- When you want session-specific overrides during development
- When building complex deployment scenarios with environment-specific settings

</details>

<details>
<summary><strong>File Naming Conventions</strong></summary>

Using the `case_format` argument, several functions of the package supports multiple naming conventions for configuration files:

- snake_case (default): `package_config_local.yml`
- camelCase: `packageConfigLocal.yml`
- PascalCase: `PackageConfigLocal.yml`
- kebab-case: `package-config-local.yml`

Alternatively, you can also use any other names (e.g., using `yaml_file` with `get_config()` and `fn_local` and `fn_tmpl` with `create_local()`)

</details>

## License

This package is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0).
