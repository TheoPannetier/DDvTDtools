#' Plot average lineage-through-time plots
#'
#' Plots the lineage-through-time curves for a pair of sets of
#' (diversity-dependent and time-dependent) simulated trees. Each curve is
#' averaged over 1000 trees with [TreeSim::LTT.plot.gen()].
#'
#' @inheritParams params_doc
#' @author Théo Pannetier
#' @export

plot_avg_ltt <- function(para, with_extinct = FALSE) {
  assert_DDvTD_wd()
  assert_para(para)
  if(!is.logical(with_extinct)) {
    stop('with_extinct must be a logical')
  }

  # Fetch phylogenies
  phylos <- purrr::map(
    arg_sim(),
    function(sim) {
      read_sim_multiPhylo(
        sim = sim,
        para = para,
        with_extinct = with_extinct)
    })
  names(phylos) <- arg_sim()

  # Compute summaries over all LTTs
  time_seq <- seq(-para_to_pars(para)[1], 0, 1)
  summary_ltt_tbl <- phylos %>% purrr::map_dfr(
    get_summary_ltt_tbl,
    time_seq = time_seq,
    .id = "sim"
  )

  if (para == 4241) {ymax <- 120} else {ymax <- 100}

  # Main plot with average LTTs
  ltt_plot <- ggplot2::ggplot(
    summary_ltt_tbl,
    ggplot2::aes(
      # mean curves
      x = t, y = mean_n
      # quantile ribbons
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = qt_10, ymax = qt_90, fill = sim),
      alpha = 0.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(colour = sim),
      size = 1
    ) +
    ggplot2::scale_colour_manual(
      values = c("green4", "blue"),
      aesthetics = c("colour", "fill"),
      guide = FALSE
    ) +
    ggplot2::geom_hline(
      yintercept = DDvTDtools::para_to_pars(para)[4],
      color = "grey50",
      linetype = "dashed"
    ) +
    ggplot2::coord_cartesian(ylim = c(0, ymax)) +
    # ggplot2::scale_y_continuous(breaks = seq(0, ymax, by = 20)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_text(vjust = 0.5, size = 16),
      axis.title.x = ggplot2::element_text(size = 16)
    ) +
    ggplot2::labs(
      title = make_plot_title_expr(para),
      x = "Time",
      y = "Number of lineages"
    )
  return(ltt_plot)
}
