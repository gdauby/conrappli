% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rainbio.R
\name{query_rb_poly}
\alias{query_rb_poly}
\title{Extract the rainbio database from a spatial query}
\usage{
query_rb_poly(poly, only_checked_georef = TRUE)
}
\arguments{
\item{poly}{An \code{sf} object, typically a polygon obtained with \code{mapedit::drawFeatures()}.}

\item{only_checked_georef}{logical, whether filtering out occurrences with non validated georeferencing.}
}
\value{
A list with the sf of the rainbio database extracted, the polygon used to extract, a tibble with idtax
}
\description{
Extract all records of all taxa identified to species level into a dranw polygon from the rainbio database
}
\examples{
\dontrun{
poly <- mapedit::drawFeatures()
query_rb_poly(poly)
}
}
\author{
Gilles Dauby, \email{gilles.dauby@ird.fr}
}
