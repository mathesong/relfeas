#' Calculate the critical value of Cohen's D
#'
#' Calculate the critical value of D for which a given sample size will yield a significant result
#'
#' @param n1 Number of measurements in group 1
#' @param n2 Number of measurements in group 2. Optional: default is n1.
#' @param sig.level Significance level. Optional: default is 0.05
#' @param alternative Alternative hypothesis. Optional: default is "two.sided"
#'
#' @return The critical Cohen's D value
#' @export
#'
#' @examples
#' d_crit(20)
#'
d_crit <- function(n1, n2=n1, sig.level=0.05, alternative="two.sided") {
  if (!(alternative %in% c("one.sided", "two.sided"))) {
    stop("alternative should be either 'one.sided' or 'two.sided'")
  }
  sig.level <- ifelse(alternative == "two.sided", yes = sig.level / 2, no = sig.level)

  df <- n1 + n2 - 2

  t_crit <- qt(sig.level, df, lower.tail = F)

  n_term <- ((n1 + n2) / (n1 * n2)) * ((n1 + n2) / (n1 + n2 - 2))
  d_crit <- t_crit * sqrt(n_term)

  return(d_crit)

  # Formula from http://www.bwgriffin.com/gsu/courses/edur9131/content/Effect_Sizes_pdf5.pdf
}



#' Calculate the critical value of Pearson's r
#'
#' Calculate the critical value of r for which a given sample size will yield a significant result
#'
#' @param n Number of measurements-
#' @param sig.level Significance level. Optional: default is 0.05
#' @param alternative Alternative hypothesis. Optional: default is "two.sided"
#'
#' @return The critical r value
#' @export
#'
#' @examples
#' r_crit(50)
#'
r_crit <- function(n, sig.level=0.05, alternative="two.sided") {

  # solve(t = ((n-2)*(r^2/(1-r^2)) )^0.5, r)

  if (!(alternative %in% c("one.sided", "two.sided"))) {
    stop("alternative should be either 'one.sided' or 'two.sided'")
  }
  sig.level <- ifelse(alternative == "two.sided", yes = sig.level / 2, no = sig.level)

  df <- n - 2 # df for a correlation is n-2

  t_crit <- qt(sig.level, df, lower.tail = F)
  # r_crit <- t_crit / sqrt(n + t_crit^2 - 2)
  r_crit <- sqrt(t_crit ^ 2 / (t_crit ^ 2 + df))

  return(r_crit)
}



#' Calculate the number of measurements for which a Cohen's D is significant
#'
#' There's almost definitely a better way of doing this.
#'
#' @param d Cohen's D
#' @param sig.level Significance level. Optional: default is 0.05.
#' @param alternative Alternative hypothesis. Optional: default is "two.sided"
#'
#' @return The number of measurements
#' @export
#'
#' @examples
#' d2n_crit(0.5)
#'
d2n_crit <- function(d, sig.level=0.05, alternative="two.sided") {
  if (!(alternative %in% c("one.sided", "two.sided"))) {
    stop("alternative should be either 'one.sided' or 'two.sided'")
  }
  sig.level <- ifelse(alternative == "two.sided", yes = sig.level / 2, no = sig.level)

  nvals <- 3:2000
  cost <- sapply(nvals, d2n_crit_model, d = d, sig.level = sig.level) # Optimisation failed sometimes - better with grid search

  if (sum(cost > 0) == 0) {
    n_req <- "> 2000"
  } else {
    n_req <- nvals[which(cost > 0)[1]]
  }

  return(n_req)
}



#' Model for calculating the number of measurements for which a Cohen's D is significant
#'
#' There's almost definitely a better way of doing this.
#'
#' @param n Number of measurements
#' @param d Cohen's D
#' @param sig.level Significance level. Optional: default is 0.05.
#'
#' @return The difference from the significance level.
#' @export
#'
#' @examples
#' d2n_crit_model(50, 0.5)
#'
d2n_crit_model <- function(n, d, sig.level=0.05) {

  # par = n = n1.  n1=n2.  sd1=sd2.  i.e. n per group.
  tval <- d / sqrt(2 / n)
  df <- 2 * n - 2

  pval <- pt(q = tval, df = df, lower.tail = F)
  cost <- sig.level - pval
  return(cost)
}


#' Calculate the number of measurements for which a Pearson's r will be significant
#'
#' There's almost definitely a better way of doing this.
#'
#' @param r Pearson's r: correlation coefficient
#' @param sig.level Significance level. Optional: default is 0.05.
#' @param alternative Alternative hypothesis. Optional: default is "two.sided"
#'
#' @return The number of measurements for which a Pearson's r is significant
#' @export
#'
#' @examples
#' r2n_crit(0.5)
#'
r2n_crit <- function(r, sig.level=0.05, alternative="two.sided") {
  if (!(alternative %in% c("one.sided", "two.sided"))) {
    stop("alternative should be either 'one.sided' or 'two.sided'")
  }
  sig.level <- ifelse(alternative == "two.sided", yes = sig.level / 2, no = sig.level)

  # n_optim <- optimise(f = r2n_crit_model, interval = c(5,500), r=r, sig.level=sig.level)
  # n_req <- ceiling(n_optim$minimum)

  nvals <- 3:2000
  cost <- sapply(nvals, r2n_crit_model, r = r, sig.level = sig.level) # Optimisation failed sometimes - better with grid search

  if (sum(cost > 0) == 0) {
    n_req <- "> 2000"
  } else {
    n_req <- nvals[which(cost > 0)[1]]
  }

  return(n_req)
}


#' Model for calculating the number of measurements for which a Pearson's r will be significant
#'
#' @param n Number of measurements
#' @param r Pearson's r: correlation coefficient
#' @param sig.level Significance level. Optional: default is 0.05.
#'
#' @return The difference from the significance level.
#' @export
#'
#' @examples
#' r2n_crit_model(50, 0.5)
#'
r2n_crit_model <- function(n, r, sig.level=0.05) {

  # par = n.
  tval <- sqrt((n - 2) * (r ^ 2 / (1 - r ^ 2)))

  # tval <- r / sqrt( (1-r^2)/(n-2) )
  #
  df <- n - 2 # df for a correlation is n-2

  pval <- pt(q = tval, df = df, lower.tail = F)
  cost <- sig.level - pval
  return(cost)
}


#' Power analysis summary for a given Cohen's D value
#'
#' @param d Cohen's D
#' @param sig.level Significance level. Optional: default is 0.05
#' @param alternative Alternative hypothesis. Optional: default is "two.sided"
#'
#' @return A data.frame with the sample sizes for different levels of power.
#' @export
#'
#' @examples
#' d2n_powersummary(d=0.8)
#'
d2n_powersummary <- function(d, sig.level=0.05, alternative="two.sided") {
  if (!(alternative %in% c("one.sided", "two.sided"))) {
    stop("alternative should be either 'one.sided' or 'two.sided'")
  }

  crit <- d2n_crit(d = d, sig.level, alternative)

  if (alternative == "one.sided") alternative <- "greater"
  pwr50 <- pwr::pwr.t.test(d = d, sig.level = sig.level, alternative = alternative, power = 0.5)$n
  pwr80 <- pwr::pwr.t.test(d = d, sig.level = sig.level, alternative = alternative, power = 0.8)$n
  pwr90 <- pwr::pwr.t.test(d = d, sig.level = sig.level, alternative = alternative, power = 0.9)$n

  out <- data.frame(d = d, Outcome = "n", crit = crit, pwr50 = pwr50, pwr80 = pwr80, pwr90 = pwr90)
  return(out)
}

#' Power analysis summary of Cohen's D values for a given sample size
#'
#' @param n1 Number of measurements in group 1.
#' @param n2 Number of measurements in group 2. Optional: defaults to n1.
#' @param sig.level Significance level. Optional: default is 0.05
#' @param alternative Alternative hypothesis. Optional: default is "two.sided"
#'
#' @return A data.frame with the Cohen's D effect sizes for different levels of power.
#' @export
#'
#' @examples
#' n2d_powersummary(n1=20)
n2d_powersummary <- function(n1, n2=n1, sig.level=0.05, alternative="two.sided") {
  if (!(alternative %in% c("one.sided", "two.sided"))) {
    stop("alternative should be either 'one.sided' or 'two.sided'")
  }

  crit <- d_crit(n1 = n1, n2 = n2, sig.level = sig.level, alternative = alternative)

  if (n1 != n2) warning("Power calculations performed assuming equal group size of (n1+n2)/2")
  n <- (n1 + n2) / 2

  if (alternative == "one.sided") alternative <- "greater"
  pwr50 <- pwr::pwr.t.test(n = n, sig.level = sig.level, alternative = alternative, power = 0.5)$d
  pwr80 <- pwr::pwr.t.test(n = n, sig.level = sig.level, alternative = alternative, power = 0.8)$d
  pwr90 <- pwr::pwr.t.test(n = n, sig.level = sig.level, alternative = alternative, power = 0.9)$d

  out <- data.frame(n1 = n1, n2 = n2, Outcome = "d", crit = crit, pwr50 = pwr50, pwr80 = pwr80, pwr90 = pwr90)
  return(out)
}

#' Power analysis summary for a given Pearson's r value
#'
#' @param r Pearson's r: correlation coefficient
#' @param sig.level Significance level. Optional: default is 0.05
#' @param alternative Alternative hypothesis. Optional: default is "two.sided"
#'
#' @return A data.frame with the power sample sizes for a given effect size.
#' @export
#'
#' @examples
#' r2n_powersummary(0.5)
#'
r2n_powersummary <- function(r, sig.level=0.05, alternative="two.sided") {
  if (!(alternative %in% c("one.sided", "two.sided"))) {
    stop("alternative should be either 'one.sided' or 'two.sided'")
  }

  crit <- r2n_crit(r = r, sig.level, alternative)

  pwr50 <- pwr::pwr.r.test(r = r, sig.level = sig.level, alternative = alternative, power = 0.5)$n
  pwr80 <- pwr::pwr.r.test(r = r, sig.level = sig.level, alternative = alternative, power = 0.8)$n
  pwr90 <- pwr::pwr.r.test(r = r, sig.level = sig.level, alternative = alternative, power = 0.9)$n

  out <- data.frame(r = r, Outcome = "n", crit = crit, pwr50 = pwr50, pwr80 = pwr80, pwr90 = pwr90)
  return(out)
}

#' Power analysis summary of Pearson's r values for a given sample size
#'
#' @param n Number of measurements
#' @param sig.level Significance level. Optional: default is 0.05
#' @param alternative Alternative hypothesis. Optional: default is "two.sided"
#'
#' @return A data.frame with the effect sizes for a given sample size.
#' @export
#'
#' @examples
#' n2r_powersummary(50)
#'
n2r_powersummary <- function(n, sig.level=0.05, alternative="two.sided") {
  if (!(alternative %in% c("one.sided", "two.sided"))) {
    stop("alternative should be either 'one.sided' or 'two.sided'")
  }

  crit <- r_crit(n, sig.level, alternative)

  pwr50 <- pwr::pwr.r.test(n = n, sig.level = sig.level, alternative = alternative, power = 0.5)$r
  pwr80 <- pwr::pwr.r.test(n = n, sig.level = sig.level, alternative = alternative, power = 0.8)$r
  pwr90 <- pwr::pwr.r.test(n = n, sig.level = sig.level, alternative = alternative, power = 0.9)$r

  out <- data.frame(n = n, Outcome = "r", crit = crit, pwr50 = pwr50, pwr80 = pwr80, pwr90 = pwr90)
  return(out)
}

#' Attenuation factor for correlation coefficients based on Spearman's equation
#'
#' @param ... A vector of reliability scores of the measures included
#'
#' @return The degree to which a correlation coefficient is attenuated by the measurement error
#' @export
#'
#' @examples
#' r_attenuation(0.8, 0.7, 0.7)
#'
r_attenuation <- function(...) {
  r_obs <- sqrt(prod(...))
  return(r_obs)
}

#' Calculate the maximum Cohen's D based on Spearman's equation
#'
#' @param ... A vector of reliability scores of the measures included
#'
#' @return The maximum Cohen's D value for a given reliability
#' @export
#'
#' @examples
#' dmax_attenuation(0.8, 0.7)
dmax_attenuation <- function(...) {
  r_obs <- r_attenuation(...)
  d_obs <- 2 * r_obs / sqrt(1 - r_obs ^ 2)
  return(d_obs)
}

#' Calculate the Cohen's D after attenuation based on Spearman's equation
#'
#' Enter in only one effect size measure
#'
#' @param rel_total Reliability or reliabilities for the total combined sample of measurements
#' @param rel_onegroup Reliability or reliabilities for one group of measurements.
#' @param d True Cohen's D of biological signal before accounting for measurement error
#' @param overlap True distributional overlap of biological signal before accounting for measurement error
#' @param u3 True Cohen's U3 of biological signal before accounting for measurement error
#' @param cles True CLES of biological signal before accounting for measurement error
#' @param n1 Number of measurements in group 1. Optional: only required if group sizes differ.
#' @param n2 Number of measurements in group 2. Optional: only required if group sizes differ.
#'
#' @return The Cohen's D after attenuation
#' @export
#'
#' @examples
#' d_attenuation(rel_total = 0.7, u3=0.9)
#' d_attenuation(rel_onegroup = 0.7, u3=0.9)

d_attenuation <- function(rel_total = NULL, rel_onegroup = NULL, d=NULL, overlap=NULL, u3=NULL, cles=NULL, n1=10, n2=n1) {
  if (sum(c(is.null(rel_total), is.null(rel_onegroup))) != 1) stop("Please specify either total or one group reliability")

  if (sum(c(is.null(overlap), is.null(u3), is.null(d), is.null(cles))) != 3) stop("Please specify only one effect size measure")

  effect_sizes <- cohend_convert(d, overlap, u3, cles)

  if (is.null(rel_total)) {
    if (length(rel_onegroup) > 1) rel_onegroup <- prod(rel_onegroup)
    semval <- icc2sem(rel_onegroup, sd = 1)
    ss_total <- sumStat_total(n1 = n1, mean1 = 0, sd1 = 1, n2 = n2, sd2 = 1, d = effect_sizes$d)
    sd_total <- sqrt(ss_total$sd_total ^ 2 + semval ^ 2)
    sd_original <- sqrt(1 + semval ^ 2)

    rel_total <- sd2extrapRel(sd = sd_total, icc_original = rel_onegroup, sd_original = sd_original)
  }

  d_true <- effect_sizes$d

  a <- (n1 + n2) ^ 2 / (n1 * n2) # Correction factor: https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
  r_true <- d_true / sqrt(d_true ^ 2 + a)

  r_obs <- r_true * r_attenuation(rel_total)

  d_obs <- 2 * r_obs / sqrt(1 - r_obs ^ 2)
  return(d_obs)
}
