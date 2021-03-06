% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jfids.R
\name{jfshortname_forfile}
\alias{jfshortname_forfile}
\title{Return a shortened (but unique) janelia image name for a path}
\usage{
jfshortname_forfile(file, checkExists = FALSE)
}
\arguments{
\item{file}{Path to file(s) to test}

\item{checkExists}{Check that calculated image stem exists (TODO)}
}
\value{
Character vector of image names (or NA if checkExists fails)
}
\description{
NB Does not include file extension
}
\seealso{
\code{\link{jfimagestem_forfile}}
}

