% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ideea_figures.R
\name{ideea_snapshot}
\alias{ideea_snapshot}
\title{Plot or return data for hourly operation of electricity sector}
\usage{
ideea_snapshot(scen, YEAR, SLICE, return_data = FALSE)
}
\arguments{
\item{scen}{solved scenario object}

\item{YEAR}{integer vector of years to return}

\item{SLICE}{character vector of time-slices to return}

\item{return_data}{logical, if TRUE, the data will be return, or gglot object otherwise.}
}
\value{
ggplot object or data.table with data for the figure.
}
\description{
Plot or return data for hourly operation of electricity sector
}
\examples{
# all_slices <- scen_CAP@settings@calendar@timetable$slice
# ideea_snapshot(scen_CAP, YEAR = 2055, SLICE = all_slices[grepl("d043", all_slices)])
# ideea_snapshot(scen_CAP, YEAR = 2055, SLICE = all_slices[grepl("d015", all_slices)])
}
