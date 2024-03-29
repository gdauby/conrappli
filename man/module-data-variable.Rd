% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module-data-variable.R
\name{module-data-variable}
\alias{module-data-variable}
\alias{data_variable_ui}
\alias{data_variable_server}
\title{Variable selection Module}
\usage{
data_variable_ui(id)

data_variable_server(id, data_r = reactive(NULL))
}
\arguments{
\item{id}{Module's ID.}

\item{data_r}{A \code{reactive} function returning a \code{data.frame}.}
}
\value{
\itemize{
\item UI: HTML tags that can be included in the UI part of the application.
\item Server: a \code{\link[shiny:reactive]{shiny::reactive()}} function returning a \code{list}.
}
}
\description{
Variable selection Module
}
