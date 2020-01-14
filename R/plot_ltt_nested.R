#' Plot n distribution inside a ltt plot
#'
#' Calls \code{plot_avg_ltt} and \code{plot_n_distrib}, and nests the latter in
#' the former
#'
#' @inheritParams params_doc
#'
#' @author Théo Pannetier
#' @export
plot_ltt_nested <- function(para) {

  crown_age <- para_to_pars(para)[1]
  lambda <-  para_to_pars(para)[2]
  mu <-  para_to_pars(para)[3]
  title <- bquote(
    "Age"       ~ "="  ~ .(crown_age) ~
      lambda[0] ~ "="  ~ .(lambda)    ~
      mu[0]     ~ "="  ~ .(mu)
  )

  n_distrib <- plot_n_distrib(para) %>%
    ggplot2::ggplotGrob()

  ltt <- plot_avg_ltt(para)

  ltt_nested <- ltt + ggplot2::annotation_custom(
    n_distrib,
    xmin = -(crown_age * 0.9), xmax = -(crown_age / 2 * 0.9),
    ymin = max(45, crown_age * (mu > 0)), # dirty hack for 4241 (60 myr) so it
    ymax = max(100, crown_age * 2 * (mu > 0)) # has the same scale as other plots
  )
  ltt_nested
}
