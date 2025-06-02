# icy

[![R-CMD-check](https://github.com/macroverse-r/icy/workflows/R-CMD-check/badge.svg)](https://github.com/macroverse-r/icy/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/icy)](https://CRAN.R-project.org/package=icy)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)

## Overview

`icy` (Interface for Configuration using YAML) is an R package that provides a comprehensive interface for managing configuration in R packages using YAML files. This package makes it easier for R packages to configure and manage environment variables, settings, and preferences through YAML configuration files, supporting both interactive setup and automated workflows.

### Key Features

- **Multi-source configuration**: Load from template, local config, or .Renviron files with priority resolution
- **Flexible file discovery**: Support for different YAML file naming conventions (snake_case, camelCase, PascalCase, kebab-case)
- **Environment variable management**: Write, update, and erase variables in .Renviron files
- **Session synchronization**: Sync environment variables between .Renviron and current R session
- **Validation**: Validate environment variable names against package templates
- **User-friendly utilities**: Toggle debug/verbose modes, display configurations, and more

## Core Functions by Category

### 🚀 Setup Functions (Package Initialization)
| Function | Description |
|----------|-------------|
| `create_local()` | Create local configuration file from template (used in `.onLoad()`) |
| `load_config()` | Load configuration into R session environment variables (used in `.onLoad()`) |

### 📖 Configuration Reading Functions
| Function | Works With | Description |
|----------|------------|-------------|
| `get_config()` | Template, Local Config, .Renviron | Load configuration from any source with priority resolution |
| `find_local()` | Local Config | Find existing local configuration files |
| `find_template()` | Template | Find package template configuration files |

### ✏️ Local Configuration Management
| Function | Description |
|----------|-------------|
| `write_local()` | Write/update variables in local YAML configuration file |

### 🌍 .Renviron File Management
| Function | Description |
|----------|-------------|
| `write_renviron()` | Write/update variables in user's .Renviron file |
| `erase_renviron()` | Remove variables from .Renviron file |

### 🔄 R Session Environment Management
| Function | Description |
|----------|-------------|
| `sync()` | Synchronize environment variables between files and current R session |

### 🔧 Toggle & Configuration Utilities
| Function | Description |
|----------|-------------|
| `toggle_debug()` | Toggle package debug mode in local configuration |
| `toggle_verbose()` | Toggle package verbose mode in local configuration |

### 📊 Display & Validation Functions
| Function | Description |
|----------|-------------|
| `show_config()` | Display current configuration values and their sources |
| `validate()` | Validate variable names against package template |

### 🔍 Package Discovery Functions
| Function | Description |
|----------|-------------|
| `get_package_name()` | Automatically detect calling package name |
| `get_package_path()` | Get package directory path (user or installation) |
| `get_renviron_path()` | Get path to user's .Renviron file |

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

## Quick Start: Understanding icy's Approach

The `icy` package manages configuration through a **three-layer system**:

1. **📋 Template** (read-only blueprint) - Defines all possible variables and their defaults
2. **📝 Local Config** (user-customizable) - User's package-specific settings 
3. **🌍 .Renviron** (global environment) - System-wide environment variables

### Understanding the Configuration Flow

```
Template (inst/package_config_template.yml)
    ↓ (copied on first use)
Local Config (~/.local/share/R/package/package_config_local.yml)
    ↓ (priority resolution)
.Renviron (~/.Renviron)
    ↓ (loaded into)
R Session Environment Variables
```

### Step 1: Create Your Package Template

First, define **all possible configuration variables** for your package. Create a template YAML file in your package's `inst/` directory (e.g., `inst/dummy_config_template.yml`):

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

**Why this matters:** The template serves as documentation for users AND validation for your package.

### Step 2: Choose Your Package Strategy

You have **two main approaches** for using `icy` in your package:

#### Strategy A: Local Config Files (Recommended)
**When to use:** You want users to have persistent, customizable settings without polluting their global environment.

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
```

#### Strategy B: R Session Environment Variables
**When to use:** You want traditional `Sys.getenv()` / `Sys.setenv()` behavior.

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

### Step 3: Understanding What Happens for Users

#### First Time User Experience:
1. User installs and loads your package
2. `icy::create_local()` copies template to their local config directory
3. User can now customize settings without affecting other users or projects

#### Ongoing Usage:
```r
# User can see current settings
icy::show_config(package = "dummy")

# User can modify local settings
icy::write_local(
  package = "dummy",
  var_list = list(DUMMY_API_KEY = "my-real-key")
)

# User can set global settings (affects all projects)
icy::write_renviron(
  package = "dummy",
  var_list = list(DUMMY_API_KEY = "global-key")
)

# Changes take effect immediately in session environment
icy::sync(package = "dummy")
```

## Usage Patterns: Real-World Scenarios

### Pattern 1: "I want to see what's currently configured"

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

**💡 Educational note:** When developing, you can use `show_config()` frequently. It's your "dashboard" for understanding configuration state.

### Pattern 2: "I'm setting up the package for the first time"

**As a package developer**, you want to make it easy for users:

```r
# User loads your package for the first time
library(dummy)  # Your .onLoad() runs icy::create_local()

# Check what was created
icy::show_config(package = "dummy")
# Shows template values because local config was just created

# User customizes their local settings
icy::write_local(
  var_list = list(
    DUMMY_API_KEY = "my-real-api-key-from-vendor",
    DUMMY_DATA_DIR = "/my/project/data",
    DUMMY_VERBOSE = FALSE  # I don't need chatty output
  ),
  package = "dummy"
)

# Settings are now persistent and package-specific
```

**💡 Educational note:** Local config changes only affect this package, not other R packages or projects.

**🔧 Developer note:** By default, `icy` detects with you are in a R package or not and behaves accordingly. Specifically, the local config with be saved in the same folder than the template if you R session is in the R package and in the user-specific config folder (e.g., ~/.config/R on Linux) in a standard R session.

### Pattern 3: "I want global settings across all my projects"

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

**💡 Educational note:** .Renviron changes affect your entire R environment. Use sparingly!

### Pattern 4: "I need different settings for different environments"

Working with development, testing, and production:

```r
# Development setup
dev_config <- get_config(
  package = "dummy",
  user = "development",  # Uses the "development" section from template
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

**💡 Educational note:** Template sections are read-only. User customizations always go in "default" section of local/renviron.

### Pattern 5: "I made changes and need them active now"

Configuration changes don't automatically affect the current R session:

```r
# Before changes
api_key_before <- get_config(package = "dummy")$DUMMY_API_KEY
print(api_key_before)  # "old-key"

# Make changes
write_local(
  var_list = list(DUMMY_API_KEY = "new-key"),
  package = "dummy"
)

# Configuration files updated, but R session not yet
api_key_still_old <- get_config(package = "dummy")$DUMMY_API_KEY
print(api_key_still_old)  # Still "old-key"

# Sync to make changes active
sync(package = "dummy")

# Now it's updated
api_key_new <- get_config(package = "dummy")$DUMMY_API_KEY
print(api_key_new)  # "new-key"
```

**💡 Educational note:** Always `sync()` after making changes if you need them immediately!

### Pattern 6: "I want to reset everything back to defaults"

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

### Pattern 7: "I'm debugging configuration issues"

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

**💡 Educational note:** Use `toggle_verbose()` and `toggle_debug()` when configuration isn't behaving as expected or during development phase.

## Configuration Priority

The package follows a clear priority hierarchy:

1. **`.Renviron`** - Highest priority (global user settings)
2. **Local Config** - Medium priority (package-specific user settings)

When using `origin = "priority"`, values from higher priority sources override lower priority ones.

## File Naming Conventions

Using the `case_format` argument, several functions of the package supports multiple naming conventions for configuration files:

- **snake_case** (default): `package_config_local.yml`
- **camelCase**: `packageConfigLocal.yml`
- **PascalCase**: `PackageConfigLocal.yml`
- **kebab-case**: `package-config-local.yml`

Alternatively, you can also use any other names (e.g., using `yaml_file` with `get_config()` and `fn_local` and `fn_tmpl` with `create_local()`)

## License

This package is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0).
