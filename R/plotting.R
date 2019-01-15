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
  overlap <- effect_sizes$overlap
  u3 <- effect_sizes$u3
  cles <- effect_sizes$cles


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
      subtitle = glue::glue("Cohen's D = {round(d,digits_d)}, ",
                            "Overlap = {round(overlap*100, digits_ol)}%,\n",
                            "Cohen's U3 = {round(u3*100, digits_u3)}%, ",
                            "CLES = {round(cles*100, digits_cles)}%")
    ) +
    theme_bw() +
    scale_x_discrete(breaks = NULL)

  return(ovplot)
}
