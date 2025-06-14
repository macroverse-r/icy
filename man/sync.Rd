% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sync.R
\name{sync}
\alias{sync}
\title{Sync Environment Variables with Configuration}
\usage{
sync(
  package = get_package_name(),
  var_names = NULL,
  user = "default",
  verbose = FALSE
)
}
\arguments{
\item{package}{Character string with the package name. Defaults to \code{get_package_name()} to detect the calling package.}

\item{var_names}{Optional character vector of specific variable names to sync.
If NULL (default), syncs all variables defined in the configuration.}

\item{user}{Character string for the user/section in the YAML file (default: "default").}

\item{verbose}{Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.}
}
\value{
Invisibly returns TRUE.
}
\description{
Updates the current R session's environment variables to match the configuration
values, respecting the priority hierarchy (.Renviron > local config). This function
is useful after modifying configuration files to apply changes without restarting R.
}
\details{
\code{sync()} will update existing variables to match the configuration values.
}
\examples{
\dontrun{
# After modifying local config, sync all variables
sync("mypackage")

# Sync specific variables only
sync("mypackage", var_names = c("API_KEY", "DB_HOST"))

# Sync quietly
sync("mypackage", verbose = FALSE)
}

}
