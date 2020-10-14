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

  phylos <- list(
    "DD" = read_sim_multiPhylo("DD", para),
    "TD" = read_sim_multiPhylo("TD", para)
  )

  n_tbl <- purrr::map(
    arg_sim(),
    function(sim) {
      read_optim_results(sim, "DD", para) %>% dplyr::select(sim, ntips)
    }
  ) %>% dplyr::bind_rows()

  variances <- n_tbl %>%
    dplyr::group_by(sim) %>%
    dplyr::summarise(
      "var" = var(ntips) %>% round(1)
    )

  n_plot <- n_tbl %>% ggplot2::ggplot(
    ggplot2::aes(x = sim, y = ntips, fill = sim)
  ) +
    ggplot2::geom_violin(scale = "width") +
    ggplot2::scale_fill_manual(values = c("green4", "blue"), guide = FALSE) +
    ggplot2::geom_hline(
      yintercept = para_to_pars(para)[4], color = "grey50", linetype = "dashed"
    ) +
    # ggplot2::coord_cartesian(
    #   ylim = c(1, max(n_tbl$ntips) * 3.2)
    # ) +
    ggplot2::scale_y_log10() +
    ggplot2::ylab("Number of tips") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank()
    ) +
    # Variance labels
    # ggplot2::geom_text(
    #   data = variances,
    #   ggplot2::aes(
    #     x = factor(sim),
    #     y = max(n_tbl$ntips) * 1.5,
    #     label = paste("sigma[", sim, "]^2 ==", var)
    #   ),
    #   size = 3,
    #   nudge_x = 0.1,
    #   nudge_y = 0.2,
    #   angle = 45,
    #   parse = TRUE
    # ) +
    ggplot2::labs(
      title = bquote(sigma[DD]^2 == .((variances$var[1])) ~~~ sigma[TD]^2 == .((variances$var[2])))
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10))

  return(n_plot)
}
