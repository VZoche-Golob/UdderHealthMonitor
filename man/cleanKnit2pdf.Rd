% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/old_functions.R
\name{cleanKnit2pdf}
\alias{cleanKnit2pdf}
\title{Knitting really clean to pdf}
\usage{
cleanKnit2pdf(infile, outfile, useEnvir)
}
\arguments{
\item{infile}{A string defining the input file with complete path.}

\item{outfile}{A string defining the output file with complete path.}

\item{useEnvir}{The environment in which the code chunks are to be evaluated
by \code{knit2pdf()}.}
}
\value{
Returns the \code{outfile} PDF file.
}
\description{
Used inside \code{\link{generate_report}}.
Converts Rnw files to PDF using \code{\link[knitr]{knit2pdf}} without leaving any other files.
}

