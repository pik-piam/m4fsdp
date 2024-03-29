% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getScenarios.R
\name{getScenarios}
\alias{getScenarios}
\title{getScenarios}
\usage{
getScenarios(tableType = NULL, oldformat = FALSE)
}
\arguments{
\item{tableType}{select the scenarios based on csv file figureTableType.csv in inst/extdata folder. NULL (default) returns scenario names.}

\item{oldformat}{return in format modelrun::bundle|scenarioname}
}
\value{
named vector
}
\description{
Scenarios for FSDP plotting
}
\details{
includes scenarios and their grouping into bundles
}
\author{
Benjamin Bodirsky
}
