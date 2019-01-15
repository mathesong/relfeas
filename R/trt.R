#' Perform test-retest analysis on a data set.
#'
#' This conducts a test-retest analysis of a data set in long (tidy) format.
#' This can be used with dplyr pipelines.  However the variable names must be
#' quoted.
#'
#' @param data The total data frame
#' @param values Column of the measured values (quoted character)
#' @param cases Column of the individual values, i.e. individuals/measured items
#'   (character)
#' @param rater Optional. Column of the rater/which measurement, i.e.
#'   measurement 1 and measurement, or raters 1, 2 and 3. If this is not
#'   specified, the function will assume that the values are in order by rater,
#'   or that it the order doesn't matter.
#'
#' @return List containing all the data, including a tibble called tidy, with
#'   the summary in a single row. icc is the ICC(A,1), and icc_l and icc_u are the
#'   upper and lower bounds of the 95% confidence interval for the icc. wcsv
#'   is the within-subject coefficient of variation, sdd is the smallest
#'   detectable difference (sddm is normalised to the mean), absvar is the
#'   average absolute variability. signvar is the signed variability (for
#'   detecting bias between measurements) and signvar_sd is the sd of the
#'   signed variability (for within-subjects power analyses), both normalised
#'   to the grand mean. All percentage values are not multiplied by 100.
#' @export
#'
#' @examples
#' library(tidyverse)
#' data("petVT", package = 'agRee')
#' amy <- as.data.frame(petVT$amygdala)
#' amy_l <- gather(amy) %>%
#'   mutate(indiv = rep(paste0('s', 1:11), times=2))
#'
#' trt(amy_l, 'value', 'indiv', 'key')
#'
trt <- function(data, values, cases, rater = NULL) {
  widedat <- trt_widify(data, values, cases, rater)
  widemat <- as.matrix(widedat[, -1])

  # Summary stats
  sumstats_rater <- tibble::as_tibble(do.call("rbind", apply(widemat, 2, psych::describe)))
  sumstats_total <- tibble::as_tibble(psych::describe(c(widemat)))

  sumstats <- rbind(sumstats_rater, sumstats_total)
  sumstats$cov <- sumstats$sd / sumstats$mean
  sumstats$grouping <- c(colnames(widemat), "All")


  # Test-Retest - ICC-based
  icc_detailed <- psych::ICC(widemat)

  icca1 <- list(value = psych::ICC(widemat)$results$ICC[2],
                lbound = psych::ICC(widemat)$results$`lower bound`[2],
                ubound = psych::ICC(widemat)$results$`upper bound`[2])  # ICC(A,1)
  wscv <- agRee::agree.wscv(widemat) # Error se scaled to mean

  sigma_e <- purrr::map(wscv, ~ . * sumstats_total$mean) # Error se estimated directly
  sem <- sumstats_total$sd * sqrt(1 - icca1$value) # Error se estimated indirectly

  sdd <- agRee::agree.sdd(widemat)
  sddm <- agRee::agree.sddm(widemat)


  # Test-Retest - AbsVar
  absvar_means <- rowMeans(widemat)
  absvar_diffs <- apply(
    widemat, 1,
    function(x) mean(as.numeric(dist(x)))
  )
  absvars <- absvar_diffs / absvar_means
  absvar <- mean(absvars)


  # Test-Retest - Var
  signvar_vals <- apply(widemat, 1, function(x) diff(x))
  if(ncol(widemat)<3) {
    signvar_means <- signvar_vals  / absvar_means
  } else {
    signvar_means <- colMeans(signvar_vals)  / absvar_means
  }

  signvar_mean <- mean( signvar_means )
  signvar_sd <- sd(signvar_means)


  tidyout <- tibble(

    # Numerical
    mean = tail(sumstats$mean, 1),
    sd = tail(sumstats$sd, 1),
    cov = tail(sumstats$cov, 1),

    # Distributional
    skew = tail(sumstats$skew, 1),
    kurtosis = tail(sumstats$kurtosis, 1),

    # Test-Retest
    icc = icca1$value,
    icc_l = icca1$lbound,
    icc_u = icca1$ubound,

    wscv = wscv$value,
    sdd = sdd$value,

    # Change
    absvar = absvar,
    signvar = signvar_mean,
    signvar_sd = signvar_sd
  )

  out <- list(
    tidy = tidyout,
    sumstats = sumstats,
    icc_detailed = icc_detailed,
    icc = icca1,
    wscv = wscv,
    sigma_e = sigma_e,
    sdd = sdd,
    sddm = sddm,
    absvar = absvar,
    absvars = absvars,
    signvar = signvar_mean,
    signvar_sd = signvar_sd,
    signvars = signvar_vals,
    means = absvar_means
  )

  return(out)
}



#' Long to wide format of test-retest data
#'
#' Convenience function to turn long-format test-retest data to wide format.
#'
#' @param data The total data frame
#' @param values Column of the measured values (quoted character)
#' @param cases Column of the individual values, i.e. individuals/measured items
#'   (character)
#' @param rater Optional. Column of the rater/which measurement, i.e.
#'   measurement 1 and measurement, or raters 1, 2 and 3. If this is not
#'   specified, the function will assume that the values are in order by rater,
#'   or that it the order doesn't matter.
#'
#' @return A tibble containing the test-retest data in wide format.
#' @export
#'
#' @examples
#' library(tidyverse)
#' data("petVT", package = 'agRee')
#' amy <- as.data.frame(petVT$amygdala)
#' amy_l <- gather(amy) %>%
#'   mutate(indiv = rep(paste0('s', 1:11), times=2))
#'
#' trt_widify(amy_l, 'value', 'indiv', 'key')
#'
#'
trt_widify <- function(data, values, cases, rater = NULL) {

  # Checking for unequal numbers of ratings for each case
  counts <- table(data[, cases ])

  if (max(counts) != min(counts)) {
    warning("Unequal number of measurements for each case/individual.\n      --> Removing all with fewer than the maximum")

    # Removal
    sameasmax <- names(counts[counts == max(counts)])

    if (length(sameasmax) < 3) {
      stop("<3 cases/individuals remaining. Quitting.")
    }

    data <- filter(data, rlang::UQ(rlang::sym(cases)) %in% sameasmax)
    counts <- table(data[, cases ])
  }


  # Addition of rater variable if absent
  if (is.null(rater)) {
    data <- dplyr::arrange_(data, cases)
    data <- dplyr::mutate(data, rater = paste0("r", rep(1:max(counts), times = length(counts))))
    rater <- "rater"
  }


  # Renaming rater variable if present
  if (!is.null(rater)) {
    data[[rater]] <- paste0("r", as.numeric(as.factor(data[[rater]])))
  }


  # Making fixed up long frame
  longdat <- tibble::tibble(
    values = data[[values]],
    cases = data[[cases]],
    rater = data[[rater]]
  )

  # Making wide
  widedat <- tidyr::spread(longdat, rater, values)

  return(widedat)
}



# Here follows a failed attempt to have unquoted variable names.  Maybe I'll figure it out another time.
#
# trt_widify <- function( .data, values , cases , rater = NULL ) {
#
#   rater_q <- rlang::enquo(rater)
#
#   # Issue: can't check if null if not evaluated. Can't evaluate as don't know what it is yet.
#
#   if( is.null( rater_q) ) {
#
#     cases_q <- rlang::enquo(cases)
#
#     # Unequal numbers
#     counts <- table( dplyr::select(.data, rlang::UQ( cases_q)) )
#
#     if( max(counts) != min(counts)) {
#
#       warning('Unequal number of measurements for each case/individual.
#                 Removing all with fewer than the maximum')
#
#       # Removal
#       sameasmax <- names(counts[counts == max(counts)])
#
#       if( length(sameasmax) < 3 ) {
#         stop('<3 cases/individuals remaining. Quitting.')
#       }
#
#       .data <- filter(.data, rlang::UQ(cases) %in% sameasmax )
#     }
#
#     # Addition of rater variable
#     .data <- arrange(.data, rlang::UQ(cases_q))
#     .data <- mutate( .data, rater = paste0('m_', rep( 1:max(counts), times=length(counts)) ) )
#
#     rater = rlang::sym('rater')
#   }
#
#   cases_q <- rlang::enquo(cases)
#   values_q <- rlang::enquo(values)
#   rater_q <- rlang::enquo(rater)
#
#   longdat <- select(.data,
#                     values = rlang::UQ(values_q),
#                     cases = rlang::UQ(cases_q),
#                     rater = rlang::UQ(rater_q) )
#
#
#   widedat <- tidyr::spread(longdat, rater, values)
#
#   return(widedat)
# }
#
#
