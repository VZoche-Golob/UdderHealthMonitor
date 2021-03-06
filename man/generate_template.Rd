% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/old_functions.R
\name{generate_template}
\alias{generate_template}
\title{Generation of a pretty UdderHealthReport template}
\usage{
generate_template(template, output = "text", ...)
}
\arguments{
\item{template}{A sweave file (.Rnw) to be used as template.}

\item{output}{One of \code{c("text", "character")}.}

\item{\dots}{Further arguments passed to \code{\link[base]{readLines}}.}
}
\value{
For \code{output = "text"} the template is printed notated as string on the console.
 Then the output can be copied and pasted into a function’s code
 (for generating the packages default).
 For \code{output = "character"}, the result of \code{readLines(template)} is returned.
}
\description{
Generates a report template for \code{\link{monitor_SCCUdderHealth}} from a Sweave-file.
  The template is internally used by \code{\link{generate_report}}.
}
\note{
Do not use \code{''} quotes in the template but \code{""} instead.
}

