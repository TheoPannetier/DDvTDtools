#' Plot the average LTTs of paired DD and TD trees
#'
#' Simultaneously plots the average ltts of a set of DD and TD trees that share
#' the same parameter set.
#'
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values.
#' @param with_extinct logical. \code{with_extinct = F} for the reconstructed
#' tree, \code{with_extinct = T} for the complete tree.
#'
#' @author Théo Pannetier
#' @export

plot_avg_ltt <- function(para, with_extinct = FALSE) {
  assert_DDvTD_wd()
  assert_para(para)
  if(!is.logical(with_extinct)) {
    stop('with_extinct must be a logical')
  }
  avg_ltt_DD <- get_avg_ltt_table(
    sim = "DD",
    para = para,
    with_extinct = with_extinct
  )
  avg_ltt_TD <- get_avg_ltt_table(
    sim = "TD",
    para = para,
    with_extinct = with_extinct
  )
  avg_ltt <- rbind(avg_ltt_DD, avg_ltt_TD)

  avg_ltt_plot <- ggplot2::ggplot(
    avg_ltt,
    ggplot2::aes(x = avg_ltt$time, y = avg_ltt$avg_n, color = avg_ltt$sim)
  ) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values = c("green4", "blue"), guide = FALSE) +
    ggplot2::geom_hline(yintercept = 40, color = "grey60", linetype = "dashed") +
    ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 180, vjust = 0.5)) +
    #ggplot2::coord_cartesian(ylim = c(0, max(avg_ltt$avg_n))) +
    ggplot2::labs(x = "Time", y = "N")
  avg_ltt_plot
}