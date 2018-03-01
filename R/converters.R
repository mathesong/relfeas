#' Convert ICC values to SEM values
#'
#' @param icc ICC value
#' @param sd Standard deviation of the whole sample
#'
#' @return The SEM (standard error of measurement) value
#' @export
#'
#' @examples
#' icc2sem(icc=0.9, sd=2)
#'
icc2sem <- function(icc, sd) {
  if (icc > 1 | icc < -1) stop("ICC values should be between -1 and 1")
  sem <- sd * sqrt(1 - icc)
  return(sem)
}

#' Convert SEM values to ICC values
#'
#' @param sem SEM (standard error of measurement) value
#' @param sd standard deviation
#'
#' @return The ICC value
#' @export
#'
#' @examples
#' sem2icc(sem=0.63, sd=2)
#'
sem2icc <- function(sem, sd) {
  icc <- 1 - (sem ^ 2 / sd ^ 2)
  return(icc)
}

#' Extrapolate reliability to standard deviation
#'
#' Calculate the standard deviation required to extrapolate an ICC value to a desired ICC value
#'
#' @param icc_extrap Extrapolated ICC value (i.e. desired reliability)
#' @param icc_original Original ICC value
#' @param sd_original Original SD value. Optional: default is 1. For 1, it will be the proportional increase in the SD.
#' @param rho SEM inflation factor. Optional: default is 1. For 1, the SEM is assumed to be constant between studies.
#'
#' @return The standard deviation of a new sample for which the ICC would be equal to the desired value
#' @export
#'
#' @examples
#' extrapRel2sd(icc_extrap=0.9, icc_original=0.5)
#'
extrapRel2sd <- function(icc_extrap, icc_original, sd_original=1, rho=1) {
  if (icc_extrap > 1 | icc_extrap < -1) {
    stop("ICC values should be between -1 and 1")
  }
  if (icc_original > 1 | icc_original < -1) {
    stop("ICC values should be between -1 and 1")
  }

  sem <- rho * icc2sem(icc_original, sd_original)
  sdout <- sem / sqrt(1 - icc_extrap)
  return(sdout)
}

#' Extrapolate standard deviation to reliability
#'
#' Calculate the extrapolated reliability for a given standard deviation value
#'
#' @param sd Standard deviation in the new sample for which the reliability is being extrapolated.
#' @param icc_original ICC from the original study.
#' @param sd_original Standard deviation from the original study. Optional: default is 1. For 1, the sd argument is the proportional increase in SD.
#' @param rho SEM inflation factor. Optional: default is 1. For 1, the SEM is assumed to be constant between studies.
#'
#' @return The extrapolated ICC ICC value for the new sample
#' @export
#'
#' @examples
#' sd2extrapRel(sd=2, icc_original=0.5)
#'
sd2extrapRel <- function(sd, icc_original, sd_original=1, rho=1) {
  if (icc_original > 1 | icc_original < -1) {
    stop("ICC values should be between -1 and 1")
  }

  sem <- rho * icc2sem(icc_original, sd_original)
  iccout <- 1 - sem ^ 2 / sd ^ 2
  return(iccout)
}



#' Convert Cohen's D to overlap
#'
#' Calculate the distributional overlap percentage for a given Cohen's D effect size.
#'
#' @param d Cohen's D
#'
#' @return The percentage overlap of the group distributions
#' @export
#'
#' @references http://rpsychologist.com/d3/cohend/
#'
#' @examples
#' d2overlap(0.8)
#'
d2overlap <- function(d) {
  overlap <- 2 * pnorm(-abs(d) / 2)
  return(overlap)
}

#' Convert Cohen's D to Cohen's U3
#'
#' Calculate the percentage of each distribution which will not overlap with the
#' mean of the other distribution
#'
#' @param d Cohen's D
#'
#' @return Cohen's U3
#' @export
#'
#' @references http://rpsychologist.com/d3/cohend/
#'
#' @examples
#'
#'
d2u3 <- function(d) {
  u3 <- pnorm(d)
  return(u3)
}

#' Convert Cohen's D to Common Language Effect Size (CLES)
#'
#' Calculate the common language effect size from a Cohen's D: the probability
#' that an individual from each group selected at random will be higher or lower
#' in the expected direction than the individual from the other group.
#'
#' @param d Cohen's D
#'
#' @return The common language effect size (CLES)
#' @export
#'
#' @references http://rpsychologist.com/d3/cohend/
#'
#' @examples
#' d2cles(0.8)
#'
d2cles <- function(d) {
  cles <- pnorm(d / sqrt(2))
  return(cles)
}

#' Common language effect size to Cohen's D
#'
#' @param cles Common language effect size
#'
#' @return Cohen's D value
#' @export
#'
#' @references http://rpsychologist.com/d3/cohend/
#'
#' @examples
#' cles2d(0.8)
#'
cles2d <- function(cles) {
  d <- qnorm(cles) * sqrt(2)
  return(d)
}

#' Convert distributional overlap to Cohen's D
#'
#' Calculate the Cohen's D for the distributional overlap percentage
#'
#' @param overlap The percentage distributional overlap
#'
#' @return The distributional overlap (between 0 and 1)
#' @export
#'
#' @references http://rpsychologist.com/d3/cohend/
#'
#' @examples
#' overlap2d(0.5)
#'
overlap2d <- function(overlap) {
  if (overlap > 1 | overlap < -1) {
    stop("Overlap should be between -1 and 1")
  }

  d <- 2 * qnorm(overlap / 2, lower.tail = F)
  return(d)
}

#' Convert Cohen's U3 to Cohen's D
#'
#' Calculate the Cohen's D for a Cohen's U3: the percentage of each distribution
#' which will not overlap with the mean of the other distribution
#'
#' @param u3 Cohen's U3 (between 0 and 1)
#'
#' @return Cohen's D
#' @export
#'
#' @references http://rpsychologist.com/d3/cohend/
#'
#' @examples
#' u32d(0.4)
#'
u32d <- function(u3) {
  if (u3 > 1 | u3 < -1) {
    stop("Overlap should be between -1 and 1")
  }

  d <- abs(qnorm(u3))
  return(d)
}


#' Cohen's D Effect Size Descriptions
#'
#' Convert between different descriptions of Cohen's D. Enter only one
#' descriptor, and obtain all the others.
#'
#' @param d Cohen's D. Optional.
#' @param overlap Distributional overlap. Optional.
#' @param u3 Cohen's U3. Optional.
#' @param cles Common language effect size. Optional.
#'
#' @return List containing all the Cohen's D descriptors
#' @export
#'
#' @references http://rpsychologist.com/d3/cohend/
#'
#' @examples
#' es_convert(d=0.8)
#' es_convert(u3=0.5)
#'
es_convert <- function(d=NULL, overlap=NULL, u3=NULL, cles=NULL) {
  if (sum(c(is.null(overlap), is.null(u3), is.null(d), is.null(cles))) != 3) {
    stop("Please specify only one effect size measure")
  }

  if (is.null(d)) {
    es <- c(overlap, u3, cles)
    if (es > 1 | es < 0) {
      stop("Overlap, U3 or CLES should be between 0 and 1")
    }

    if (!is.null(u3)) d <- u32d(u3)
    if (!is.null(overlap)) d <- overlap2d(overlap)
    if (!is.null(cles)) d <- cles2d(cles)
  }

  u3 <- d2u3(d)
  overlap <- d2overlap(d)
  cles <- d2cles(d)

  out <- list(
    d = d, u3 = u3,
    overlap = overlap, cles = cles
  )

  return(out)
}
