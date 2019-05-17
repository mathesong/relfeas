#' Plot the difference between two distributions
#'
#' Visualise the difference between two distributions given an effect size measure
#'
#' @param d Cohen's D. Optional: pick only one effect size
#' @param overlap Distributional overlap.  Optional: pick only one effect size
#' @param u3 Cohen's U3.  Optional: pick only one effect size
#' @param cles Common Language Effect Size.  Optional: pick only one effect size
#' @param colours Colours of the two distributions. Optional.
#'
#' @return A ggplot2 object of the plot
#' @export
#'
#' @import ggplot2
#'
#' @references http://rpsychologist.com/d3/cohend/
#'
#' @examples
#' plot_difference(d=2)
#'
plot_difference <- function(d=NULL, overlap=NULL, u3=NULL, cles=NULL, colours=c("#85d4e3", "#e39f85"),
                            digits_d=2, digits_ol=0, digits_u3=0, digits_cles=0) {

  if (sum(c(is.null(overlap), is.null(u3), is.null(d), is.null(cles))) != 3) stop("Please specify only one effect size measure")

  effect_sizes <- cohend_convert(d, overlap, u3, cles)

  d <- effect_sizes$d
  overlap <- effect_sizes$overlap*100
  u3 <- effect_sizes$u3*100
  cles <- effect_sizes$cles*100

  ovplot <- ggplot(data.frame(x = c(-5, 5)), aes(x)) +
    stat_function(
      fun = dnorm, colour = colours[1],
      size = 1.5, args = list(mean = -d / 2), geom = "line"
    ) +
    stat_function(
      fun = dnorm, colour = colours[2],
      size = 1.5, args = list(mean = d / 2), geom = "line"
    ) +
    stat_function(
      fun = dnorm, colour = NA, fill = colours[1], alpha = 0.3,
      size = 1.5, args = list(mean = -d / 2), geom = "area"
    ) +
    stat_function(
      fun = dnorm, colour = NA, fill = colours[2], alpha = 0.3,
      size = 1.5, args = list(mean = d / 2), geom = "area"
    ) +
    geom_vline(xintercept = c(d / 2, -d / 2), linetype = "dotted") +
    labs(
      title = "Overlap between Groups",
      x = "Outcome Value", y = "Density",
      subtitle = glue::glue("Cohen's d = {round(d,digits_d)}, ",
                            "Overlap = {roundbound(overlap, digits_ol)}%,\n",
                            "Cohen's U3 = {roundbound(u3, digits_u3)}%, ",
                            "CLES = {roundbound(cles, digits_cles)}%")
    ) +
    theme_bw() +
    scale_x_discrete(breaks = NULL)

  return(ovplot)
}


#' Bounded rounding
#'
#' This function is for presenting a figure when it should not reach a certain
#' bound, and instead shows it as greater than the number below.
#'
#' @param x a numeric value.
#' @param digits integer indicating the number of decimal places
#' @param maximum maximum bound. Defaults to 100.
#' @param minimum minimum bound. Defaults to 0.
#'
#' @return A character string of the rounded value
#' @export
#'
#' @examples
#' roundbound(99.9)
#' roundbound(99.3)
#'
#' roundbound(0.9)
#' roundbound(0.3)
#'
roundbound <- function(x, digits=0, maximum=100, minimum=0) {

  if( round(x, digits+1) > (maximum - 10^(-(digits))) ) {

    x <- paste0(">",  maximum - 1*10^(-digits))

  } else if( round(x, digits+1) < (minimum + 10^(-(digits))) ) {

    x <- paste0("<",  minimum + 1*10^(-digits))

  } else {

    x <- as.character( round(x, digits) )

  }

  return(x)

}

