% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_config.R
\name{get_config}
\alias{get_config}
\title{Get Environment Variable Configuration}
\usage{
get_config(
  package = get_package_name(),
  origin = "local",
  user = "default",
  yaml_file = NULL,
  case_format = "snake_case",
  verbose = FALSE
)
}
\arguments{
\item{package}{Character string with the package name. Defaults to \code{get_package_name()} to detect the calling package.}

\item{origin}{Character string specifying where to read the configuration from:
\itemize{
\item "template": Read from the package's template YAML file (read-only blueprint)
\item "local": Read from the user's local configuration file (default)
\item "renviron": Read from .Renviron file
\item "priority": Read with priority order (.Renviron > local config)
}}

\item{user}{Character string for the user/section in the YAML file (default: "default").}

\item{yaml_file}{Character string with the name or path to the YAML file. If NULL,
the function will search for the appropriate file based on the origin.}

\item{case_format}{Character string indicating the case format to use for
searching the YAML file if \code{yaml_file} is NULL. Options are:
"snake_case" (default), "camelCase", "PascalCase", "kebab-case".}

\item{verbose}{Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.}
}
\value{
Named list of environment variable configurations.
}
\description{
Reads environment variable configuration from different origins (template, local, or .Renviron).
This function is typically used within an R package to retrieve environment variable
configurations based on the specified origin.
}
\examples{
\dontrun{
# Get configuration from template
template_config <- get_config(package = "mypackage", origin = "template")

# Get configuration from local file
local_config <- get_config(package = "mypackage", origin = "local")

# Get configuration with priority resolution
config <- get_config(package = "mypackage", origin = "priority")
}

}
