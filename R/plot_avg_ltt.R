#' Plot average lineage-through-time plots
#'
#' Plots the lineage-through-time curves for a pair of sets of
#' (diversity-dependent and time-dependent) simulated trees. Each curve is
#' averaged over 1000 trees with [TreeSim::LTT.plot.gen()].
#'
#' @inheritParams params_doc
#' @param trees a vector of trees indices. If supplied, the LTTs of these trees
#' will also be plotted.
#' @param alpha transparence level for LTTs, if `trees` is supplied.
#'
#' @author Théo Pannetier
#' @export

plot_avg_ltt <- function(para, with_extinct = FALSE, trees = NULL, alpha = 0.05) {
  assert_DDvTD_wd()
  assert_para(para)
  if(!is.logical(with_extinct)) {
    stop('with_extinct must be a logical')
  }

  phylos <- list(
    "DD" = read_sim_multiPhylo(
      sim = "DD",
      para = para,
      with_extinct = with_extinct
    ),
    "TD" = read_sim_multiPhylo(
      sim = "TD",
      para = para,
      with_extinct = with_extinct
    )
  )

  # Get ltt tbls for both DD and TD
  ltt_tbls_DD <- get_ltt_tbls(
    sim = "DD",
    para = para,
    with_extinct = with_extinct
  )
  ltt_tbls_TD <- get_ltt_tbls(
    sim = "TD",
    para = para,
    with_extinct = with_extinct
  )

  # Separate average LTTs from the rest
  avg_ltt_tbl <- dplyr::bind_rows(
    ltt_tbls_DD[[1]], ltt_tbls_TD[[1]]
  )
  # Gather the rest as a single tibble
  if (!is.null(trees)) {
    ltt_tbls_DD[[1]] <- NULL
    tbl_DD <- lapply(seq_along(ltt_tbls_DD), function(i) {
      ltt_tbls_DD[[i]] %>% dplyr::mutate("mc" = i)
    }) %>% dplyr::bind_rows()

    ltt_tbls_TD[[1]] <- NULL
    tbl_TD <- lapply(seq_along(ltt_tbls_TD), function(i) {
      ltt_tbls_TD[[i]] %>% dplyr::mutate("mc" = i)
    }) %>% dplyr::bind_rows()
  }

  # if (para == 4241) {ymax <- 120} else {ymax <- 100}

  # Main plot with average LTTs
  ltt_plot <- ggplot2::ggplot(
    avg_ltt_tbl,
    ggplot2::aes(x = time, y = N, color = sim)
  ) +
    ggplot2::scale_colour_manual(values = c("green4", "blue"), guide = FALSE) +
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
  # Add all individual LTTs in transparency
  if (!is.null(trees)) {
    ltt_plot <- ltt_plot +
      ggplot2::geom_step(
        ggplot2::aes(group = mc),
        data = tbl_DD %>% dplyr::filter(mc %in% trees),
        alpha = alpha
      ) +
      ggplot2::geom_step(
        ggplot2::aes(group = mc),
        data = tbl_TD %>% dplyr::filter(mc %in% trees),
        alpha = alpha
      )
  }
  ltt_plot <- ltt_plot +
    ggplot2::geom_line(size = 1)
  return(ltt_plot)
}
