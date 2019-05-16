#' Plot the distribution of N for DD & TD trees
#'
#' Jointly plots the distribution of the number of tips at present for a pair
#' of TD and DD trees that share the same parameter set.
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values.
#'
#' @author Théo Pannetier
#' @export
plot_n_distrib <- function(para) {
  assert_DDvTD_wd()
  assert_para(para)

  n_distrib_DD <- cbind(
    get_n_distrib(sim = "DD", para = para),
    "sim" = factor("DD", levels = get_sim_names())
  )
  n_distrib_TD <- cbind(
    get_n_distrib(sim = "TD", para = para),
    "sim" = factor("TD", levels = get_sim_names())
  )
  n_distrib <- rbind(n_distrib_DD, n_distrib_TD)

  n_plot <- ggplot2::ggplot(n_distrib, ggplot2::aes(x = n_distrib$sim, y = n_distrib$N, fill = n_distrib$sim)) +
    ggplot2::geom_violin(scale = "width") +
    ggplot2::scale_fill_manual(values = c("green4", "blue"), guide = FALSE) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(angle = 180, vjust = 0.5),
      axis.title.x = ggplot2::element_blank()) +
    ggplot2::labs(y = "N") +
    ggplot2::geom_hline(yintercept = 40, color = "grey50", linetype = "dashed")
 n_plot

 # Distribution plot
 # n_plot <- ggplot2::ggplot(n_distrib, ggplot2::aes(x = N, color = sim)) +
 #   ggplot2::geom_density() +
 #   ggplot2::scale_colour_manual(values = c("green4", "blue"), guide = FALSE) +
 #   ggplot2::geom_vline(xintercept = 40, color = "red", linetype = "dashed")
 # n_plot

}
