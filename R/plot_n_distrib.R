#' Plot the distribution of N for DD & TD trees as violin plots
#'
#' Plots the distribution of the number of tips at present for a pair of sets
#' of diversity-dependent and time-dependent simulated trees.
#'
#' @inheritParams params_doc
#'
#' @author Théo Pannetier
#' @export
#'
plot_n_distrib <- function(para) {
  assert_DDvTD_wd()
  assert_para(para)

  n_table_DD <- cbind(
    get_n_table(sim = "DD", para = para),
    "sim" = factor("DD", levels = arg_sim())
  )
  n_table_TD <- cbind(
    get_n_table(sim = "TD", para = para),
    "sim" = factor("TD", levels = arg_sim())
  )
  n_table <- rbind(n_table_DD, n_table_TD)

  n_plot <- ggplot2::ggplot(n_table, ggplot2::aes(
    x = n_table$sim, y = n_table$N, fill = n_table$sim
  )) +
    ggplot2::geom_violin(scale = "width") +
    ggplot2::scale_fill_manual(values = c("green4", "blue"), guide = FALSE) +
    ggplot2::geom_hline(
      yintercept = 40, color = "grey50", linetype = "dashed"
    ) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = make_plot_title_expr(para),
      y = "Number of tips"
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank()
    )
  n_plot
}
