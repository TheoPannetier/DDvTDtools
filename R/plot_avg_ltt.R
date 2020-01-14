#' Plot the average LTTs of paired DD and TD trees
#'
#' Plots the average LTTs of a set of DD and TD trees that share the same parameter set.
#'
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values.
#' @param with_extinct logical. \code{with_extinct = FALSE} for the
#' reconstructed tree, \code{with_extinct = TRUE} for the complete tree.
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

  if (para == 4241) {
    ymax <- 120
  } else {
    ymax <- 100
  }

  avg_ltt_plot <- ggplot2::ggplot(
    avg_ltt,
    ggplot2::aes(x = avg_ltt$time, y = avg_ltt$avg_n, color = avg_ltt$sim)
  ) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values = c("green4", "blue"), guide = FALSE) +
    ggplot2::geom_hline(
      yintercept = 40, color = "grey50", linetype = "dashed"
      ) +
    ggplot2::coord_cartesian(ylim = c(0, ymax)) +
    ggplot2::scale_y_continuous(breaks = seq(0, ymax, by = 20)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(vjust = 0.5, size = 16),
      axis.title.x = ggplot2::element_text(size = 16)
    ) +
    ggplot2::labs(x = "Time", y = "Number of tips")
  avg_ltt_plot
}
