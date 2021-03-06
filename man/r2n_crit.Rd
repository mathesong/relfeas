% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/power.R
\name{r2n_crit}
\alias{r2n_crit}
\title{Calculate the number of measurements for which a Pearson's r will be significant}
\usage{
r2n_crit(r, sig.level = 0.05, alternative = "two.sided")
}
\arguments{
\item{r}{Pearson's r: correlation coefficient}

\item{sig.level}{Significance level. Optional: default is 0.05.}

\item{alternative}{Alternative hypothesis. Optional: default is "two.sided"}
}
\value{
The number of measurements for which a Pearson's r is significant
}
\description{
There's almost definitely a better way of doing this.
}
\examples{
r2n_crit(0.5)

}
