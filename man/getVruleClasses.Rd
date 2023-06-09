% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vrule_abstract.R
\name{getVruleClasses}
\alias{getVruleClasses}
\title{getVruleClasses}
\usage{
getVruleClasses()
}
\description{
get the list of validation classes, ie classes extending \link{vrule_abstract} super class,
including classes eventually defined outside \pkg{vrule}. In case the latter is on the search path,
the list of validation classes will be cached for optimized used by \pkg{vrule} encoder/decoder.
}
\examples{
  getVruleClasses()

}
\author{
Emmanuel Blondel, \email{emmanuel.blondel1@gmail.com}
}
