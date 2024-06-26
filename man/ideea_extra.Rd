% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ideea_extra.R
\name{ideea_extra}
\alias{ideea_extra}
\title{Return path to the IDEEA extra-data directory}
\usage{
ideea_extra(subdir = NULL, filename = NULL, check = FALSE)
}
\arguments{
\item{subdir}{character, the name or path to the sub-directory in the IDEEA-extra folder. File name will also be accepted. The path must be relative to the IDEEA external folder.}

\item{filename}{character, the name of the file in the IDEEA-extra folder or subfolder if subdir is provided.}

\item{check}{logical, if TRUE, the function will report error if the directory or file doesn't exist.}
}
\value{
character, the path to the IDEEA external data directory or file.
}
\description{
Return path to the IDEEA extra-data directory
}
\examples{
ideea_extra()
ideea_extra("merra")
ideea_extra("merra", "merra_raw_2019.fst")
}
