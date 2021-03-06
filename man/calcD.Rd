% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_functions.R
\name{calcD}
\alias{calcD}
\title{Calculate Cohen's D from summary statistics}
\usage{
calcD(n1, mean1, sd1, n2 = n1, mean2, sd2 = sd1)
}
\arguments{
\item{n1}{Number of measurements in group 1.}

\item{mean1}{Mean of group 1.}

\item{sd1}{SD of group 1.}

\item{n2}{Number of measurements in group 2. Optional: defaults to n1.}

\item{mean2}{Mean of group 2.}

\item{sd2}{SD of group 2. Optional: defaults to sd1.}
}
\value{
Cohen's D
}
\description{
Calculate Cohen's D from summary statistics
}
\examples{
calcD(n1=10, mean1=10, sd1=1, mean2=11)
}
