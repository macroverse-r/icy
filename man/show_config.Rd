% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show_config.R
\name{show_config}
\alias{show_config}
\title{Show Environment Variable Status}
\usage{
show_config(
  package = get_package_name(),
  var_names = NULL,
  user = "default",
  show_source = TRUE
)
}
\arguments{
\item{package}{Character string with the package name. Defaults to \code{get_package_name()} to detect the calling package.}

\item{var_names}{Optional character vector of specific variable names to show.
If NULL (default), shows all variables defined in the configuration.}

\item{user}{Character string for the user/section in the YAML file (default: "default").}

\item{show_source}{Logical; whether to show the source of each value
(e.g., ".Renviron", "local config", "not set"). Default is TRUE.}
}
\value{
Invisibly returns a data frame with variable names, values, and sources.
}
\description{
Displays the current values and status of environment variables, including
information about where each value comes from (e.g., .Renviron, local config,
or not set). This function provides comprehensive visibility into the
configuration state.
}
\examples{
\dontrun{
# Show all environment variables for a package
show_config("mypackage")

# Show specific variables only
show_config("mypackage", var_names = c("API_KEY", "DB_HOST"))

# Show without source information
show_config("mypackage", show_source = FALSE)

# Capture the results for programmatic use
status <- show_config("mypackage")
unset_vars <- status$variable[status$source == "not set"]
}

}
