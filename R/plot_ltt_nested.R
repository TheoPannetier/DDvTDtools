#' Plot n distribution inside a ltt plot
#'
#' Plots [plot_n_distrib()] inside [plot_avg_ltt()], as Fig. 2 in
#' Pannetier et al. (2020).
#'
#' @inheritParams params_doc
#'
#' @author Théo Pannetier
#' @export
plot_ltt_nested <- function(para) {

  crown_age <- para_to_pars(para)[1]
  mu <-  para_to_pars(para)[3]

  n_distrib <- plot_n_distrib(para) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      title = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )
  n_distrib <- ggplot2::ggplotGrob(n_distrib)

  ltt <- plot_avg_ltt(para)

  ltt_nested <- ltt +
    ggplot2::annotation_custom(
    n_distrib,
    xmin = -(crown_age * 0.9), xmax = -(crown_age / 2 * 0.9),
    ymin = max(45, crown_age * (mu > 0)), # dirty hack for 4241 (60 myr) so it
    ymax = max(100, crown_age * 2 * (mu > 0)) # has the same scale as other plots
  )
  ltt_nested
}
