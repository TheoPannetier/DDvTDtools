#' Plot n distribution inside a ltt plot
#'
#' Calls \code{plot_avg_ltt} and \code{plot_n_distrib}, and nests the latter in
#' the former
#'
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values.
#'
#' @author Théo Pannetier
#' @export
plot_ltt_nested <- function(para) {
  crown_age <- para_to_pars(para)[1]
  lambda <-  para_to_pars(para)[2]
  mu <-  para_to_pars(para)[3]
  n_distrib <- plot_n_distrib(para) %>%
    ggplot2::ggplotGrob()
  ltt <- plot_avg_ltt(para) +
    ggplot2::labs(
      title = bquote(
        "Age"     ~ "="  ~ .(crown_age) ~ " " ~
        lambda[0] ~ "="  ~ .(lambda)    ~ " " ~
        mu[0]     ~ "="  ~ .(mu)
      ))
  ltt_nested <- ltt + annotation_custom(
    n_distrib,
    xmin = -crown_age, xmax = -(crown_age / 2),
    ymin = 50, ymax = 100
  )
  ltt_nested
}
