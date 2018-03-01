#' Summary statistics of combined samples
#'
#' Calculate the summary statistics over the combined groups from the individual summary statistics
#'
#' @param n1 Number of measurements in group 1
#' @param mean1 Mean of group 1
#' @param sd1 SD of group 1
#' @param n2 Number of measurements in group 2. Optional: default is n1.
#' @param mean2 Mean of group 2. Optional: this or the effect size can be specified.
#' @param sd2 SD of group 2
#' @param d Cohen's D. Optional: this or mean2 can be specified.
#' @param effect Direction of the effect. "increase" or "decrease".
#'
#' @return List containing the total n, mean and SD as well as the pooled SD, Cohen's D and mean2.
#' @export
#'
#' @examples
#' sumStat_total(n1=20, mean1=5, sd1=1, d=0.8)
#'
sumStat_total <- function(n1, mean1, sd1, n2=n1, mean2=NULL, sd2=sd1, d=NULL, effect="increase") {
  if ((is.null(mean2) & is.null(d)) | (!is.null(mean2) & !is.null(d))) {
    stop("You need to specify either mean2 or d")
  }

  if (effect != "increase" & effect != "decrease") {
    stop("effect should either be increase or decrease")
  }

  posneg <- ifelse(effect == "increase", 1, -1)

  if (is.null(d)) {
    d <- calcD(n1, mean1, sd1, n2, mean2, sd2)
  }

  sd_p <- sd_pooled(n1, sd1, n2, sd2)

  if (is.null(mean2)) {
    mean2 <- mean1 + d * sd_p * posneg
  }

  ntot <- n1 + n2

  mean_total <- mean_tot(n1, mean1, n2, mean2)

  sd_total <- sd_tot(n1, mean1, sd1, n2, mean2, sd2)

  out <- list(
    n_total = ntot, mean_total = mean_total, sd_total = sd_total,
    sd_p = sd_p, d = d, mean2 = mean2
  )
  return(out)
}

#' Calculate the pooled standard deviation
#'
#' Calculates the pooled standard deviation from summary statistics. This is not
#' the total standard deviation over groups, but the pooled standard deviation
#' used for calculation of the Cohen's D.
#'
#' @param n1 Number of measurements in group 1.
#' @param sd1 SD of group 1.
#' @param n2 Number of measurements in group 2. Optional: default is n1.
#' @param sd2 SD of group 2. Optional: default is sd1.
#'
#' @return The pooled standard deviation
#' @export
#'
#' @examples
#' sd_pooled(n1=10, sd1=3, n2=15, sd2=5)
#'
sd_pooled <- function(n1, sd1, n2=n1, sd2=sd1) {
  sd_pooled <- sqrt(((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
  return(sd_pooled)
}

#' Calculate the total mean
#'
#' Calculates the total mean over two groups.
#'
#' @param n1 Number of measurements in group 1.
#' @param mean1 Mean of group 1.
#' @param n2 Number of measurements in group 2.
#' @param mean2 Mean of group 2.
#'
#' @return The total mean.
#' @export
#'
#' @examples
#' mean_tot(n1=15, mean1=5, n2=10, mean2=3)
#'
mean_tot <- function(n1, mean1, n2=n1, mean2) {
  mean_total <- (n1 * mean1 + n2 * mean2) / (n1 + n2)
  return(mean_total)
}


#' Calculate the total standard deviation
#'
#' Calculates the total standard deviation across two groups as if they were one combined group.
#'
#' @param n1 Number of measurements in group 1.
#' @param mean1 Mean of group 1.
#' @param sd1 SD of group 1.
#' @param n2 Number of measurements in group 2. Optional: defaults to n1.
#' @param mean2 Mean of group 2. Optional: this or the effect size can be specified.
#' @param sd2 SD of group 2. Optional: defaults to sd1.
#' @param d Cohen's D. Optional: this or mean2 can be specified.
#'
#' @return Total standard deviation across two groups.
#' @export
#'
#' @examples
#' sd_tot(n1=20, mean1=1, sd1=1, mean2=5)
#'
sd_tot <- function(n1, mean1, sd1, n2=n1, mean2=NULL, sd2=sd1, d=NULL) {
  if ((is.null(mean2) & is.null(d)) | (!is.null(mean2) & !is.null(d))) {
    stop("You need to specify either mean2 or d")
  }

  if (is.null(mean2)) {
    ss_tot <- sumStat_total(n1=n1, mean1=mean1, sd1=sd1, sd2=sd2, d=d)
    mean2 <- mean1 + d * ss_tot$sd_p * posneg
  }

  if (n1 <= 0 | n2 <= 0) stop("The groups must be larger than 0")

  mean_total <- mean_tot(n1, mean1, n2, mean2)

  sd_total <- sqrt((1 / (n1 + n2 - 1)) * ((n1 - 1) * sd1 ^ 2 + n1 * mean1 ^ 2 +
    (n2 - 1) * sd2 ^ 2 + n2 * mean2 ^ 2 -
    (n1 + n2) * mean_total ^ 2))

  return(sd_total)
}

#' Calculate Cohen's D from summary statistics
#'
#' @param n1 Number of measurements in group 1.
#' @param mean1 Mean of group 1.
#' @param sd1 SD of group 1.
#' @param n2 Number of measurements in group 2. Optional: defaults to n1.
#' @param mean2 Mean of group 2.
#' @param sd2 SD of group 2. Optional: defaults to sd1.
#'
#' @return Cohen's D
#' @export
#'
#' @examples
#' calcD(n1=10, mean1=10, sd1=1, mean2=11)
calcD <- function(n1, mean1, sd1, n2=n1, mean2, sd2=sd1) {
  d <- (mean2 - mean1) / sd_pooled(n1, sd1, n2, sd2)
  return(d)
}

#' Calculate mean2 from the total SD
#'
#' @param sd_total Total SD over groups: i.e. of the combined sample.
#' @param n1 Number of measurements in group 1.
#' @param mean1 Mean of group 1. Optional: defaults to 0 if only interested in Cohen's D output.
#' @param sd1 SD of group 1. Optional: default is 1, in which case sd_total is a proportional increase in SD.
#' @param n2 Number of measurements in group 2. Optional: defaults to n1.
#' @param sd2 SD of group 2. Optional: defaults to sd1.
#'
#' @return List containing mean2 in positive and negative directions, the total
#'   mean in positive and negative directions, Cohen's D and the percentage
#'   difference.
#' @export
#'
#' @examples
#' sdtot2mean2(sd_total=1.2, n1=20)
#'
sdtot2mean2 <- function(sd_total, n1, mean1=0, sd1=1, n2=n1, sd2=sd1) {
  if (sd_total <= sd1 & sd_total <= sd2) {
    stop("sd_total cannot be smaller than both sd1 and sd2")
  }

  ntot <- n1 + n2

  term1 <- -2 * mean1 * n1 * n2

  denomterm <- 2 * (n2 ^ 2 - n2 * ntot)

  difterm1 <- 4 * n2 * (n2 - ntot)

  difterm2 <- mean1 ^ 2 * n1 ^ 2 -
    mean1 ^ 2 * n1 * ntot +
    ntot * sd1 ^ 2 -
    n1 * ntot * sd1 ^ 2 +
    ntot * sd2 ^ 2 -
    n2 * ntot * sd2 ^ 2 -
    ntot * sd_total ^ 2 +
    ntot ^ 2 * sd_total ^ 2


  difterm <- sqrt(4 * mean1 ^ 2 * n1 ^ 2 * n2 ^ 2 - difterm1 * difterm2)

  mean2neg <- (term1 + difterm) / denomterm
  mean2pos <- (term1 - difterm) / denomterm

  # Checking that they are indeed negative and positive
  if (mean2pos < mean2neg) {
    negval <- mean2pos
    mean2pos <- mean2neg
    mean2neg <- negval
  }

  meantotpos <- 1 / ntot * (n1 * mean1 + n2 * mean2pos)
  meantotneg <- 1 / ntot * (n1 * mean1 + n2 * mean2neg)

  d <- calcD(n1, mean1, sd1, n2, mean2pos, sd2)

  percdif <- 100 * ((mean2pos / mean1) - 1)

  out <- list(
    mean2pos = mean2pos,
    mean2neg = mean2neg,
    meantotpos = meantotpos,
    meantotneg = meantotneg,
    d = d, percdif = percdif
  )

  return(out)
}
