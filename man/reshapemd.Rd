% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reshapedb.R
\name{reshapemd}
\alias{reshapemd}
\title{Reshape multidynamic query}
\usage{
reshapemd(dataset, instrument = NULL, extratime = FALSE, partner = NULL,
  project = NULL, bio = TRUE)
}
\arguments{
\item{dataset:}{is the datset we want to trasform}

\item{instrument:}{the default, NULL, uses all the instruments in the file.
Otherwise is possible to add one instrument ID or an array of instrument IDs
(e.g. to have shapes management and views 'instrument = c ( 102 , 201)' )}

\item{extratime:}{if TRUE removes extratime candidates; default is FALSE}

\item{partner/project:}{The default, null, returns all partner or project accounts, but is possible to use this for filtering}

\item{bio:}{the default (TRUE), adds biodata. Set it as FALSE if yu don't want biodata}
}
\value{
Returns a tidy daframe with one person per row
}
\description{
transforms any result of a multidynamic query into a tidy dataset.
}
\examples{
reshapemd (export_from_germany,instrument = c(102,301), extratime = TRUE, project = 80254, bio = FALSE)
}
