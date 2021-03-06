% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/converters.R
\name{overlap2d}
\alias{overlap2d}
\title{Convert distributional overlap to Cohen's D}
\usage{
overlap2d(overlap)
}
\arguments{
\item{overlap}{The percentage distributional overlap}
}
\value{
The distributional overlap (between 0 and 1)
}
\description{
Calculate the Cohen's D for the distributional overlap percentage
}
\examples{
overlap2d(0.5)

}
\references{
http://rpsychologist.com/d3/cohend/
}
