% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_template.R
\name{find_template}
\alias{find_template}
\title{Find Template Configuration File}
\usage{
find_template(
  package = get_package_name(),
  fn_tmpl = NULL,
  case_format = "snake_case",
  verbose = FALSE
)
}
\arguments{
\item{package}{Character string with the package name. If NULL (default),
uses the current package name.}

\item{fn_tmpl}{Character string with custom filename pattern for the template.
If NULL, uses the default naming pattern based on case_format.}

\item{case_format}{Character string indicating the case format to use for the filename search.
Options are:
\itemize{
\item "snake_case" (default): Searches for files like "package_config_template.yml"
\item "camelCase": Searches for files like "packageConfigTemplate.yml"
\item "PascalCase": Searches for files like "PackageConfigTemplate.yml"
\item "kebab-case": Searches for files like "package-config-template.yml"
}}

\item{verbose}{Logical. If TRUE, displays informative messages about the operation. Defaults to FALSE.}
}
\value{
Character string with the full path to the found template file, or NULL
if no template is found.
}
\description{
Locates the template YAML configuration file that contains the blueprint for
environment variable definitions for a specified package. The function searches
within the package's installation directory (not user directory) for the template
configuration file.
}
\details{
Template files are read-only blueprints that define the structure and default
values for environment variables. They should be located in the package's
installation directory, typically in the inst/ folder.
}
\examples{
\dontrun{
# Find the template config file for a package
template_path <- find_template("mypackage")

# Find with custom naming pattern
template_path <- find_template("mypackage", fn_tmpl = "my_template.yml")

# Find with different case format
template_path <- find_template("mypackage", case_format = "camelCase")
}

}
