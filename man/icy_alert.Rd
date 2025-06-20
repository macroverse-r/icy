% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icy_messaging.R
\name{icy_alert}
\alias{icy_alert}
\title{Generic alert message}
\usage{
icy_alert(msg, ...)
}
\arguments{
\item{msg}{Alert message}

\item{...}{Additional arguments passed to underlying functions}
}
\description{
Generic alert function that uses contextual formatting when available.
Falls back to info alert when contextual is not available.
}
