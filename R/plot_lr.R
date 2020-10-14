#' Plot log-LR distribution for DD & TD trees
#'
#' Produces the log-LR distribution for all trees sharing the same parameter
#' set, as Fig. 3 in Pannetier et al. (2020)
#'
#' @inheritParams params_doc
#' @param plot_thresholds logical, should threshold bars be plotted with the
#' distribution?
#' @param quant_range a length-2 numeric vector, the quantiles defining the
#' range of values to be plotted
#'
#' @author Théo Pannetier
#'
#' @export

plot_lr <- function(para,
                    init_k = get_init_k()[which(names(get_init_k()) == para)],
                    plot_thresholds = TRUE,
                    quant_range = c(0, 1)) {
  assert_DDvTD_wd()
  assert_para(para)
  assert_init_k(init_k)
  if (!is.logical(plot_thresholds)) {
    stop("invalid input - plot_thresholds must be logical")
  }

  # Fetch and assemble data sets -----------------------------------------------
  optim_tbl <- lapply(arg_sim(), function(sim) {
    join_optim_tbls(sim = sim, para = para, init_k = init_k) %>%
      dplyr::mutate("sim" = sim)
  }) %>% dplyr::bind_rows()

  # Plot  ---------------------------------------------------------------
  gg <- ggplot2::ggplot(data = optim_tbl) +
    # data are plotted at the end to be overlaid on grey rect background
    ggplot2::scale_fill_manual(values = c("green4", "blue"), guide = FALSE) +
    ggplot2::coord_cartesian(
      xlim = quantile(optim_tbl$log_lr, probs = quant_range, na.rm = TRUE),
      ylim = c(0, 1)
    ) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(
      title = make_plot_title_expr(para),
      x = latex2exp::TeX("log L_{DD} - log L_{TD}"),
      y = "Density"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 14))

  # Thresholds ---------------------------------------------------------
  if (plot_thresholds == TRUE) {
    threshold_dd <- optim_tbl %>%
      dplyr::filter(sim == "DD") %>%
      dplyr::select(log_lr) %>%
      stats::quantile(probs = 0.05, na.rm = TRUE)
    threshold_td <- optim_tbl %>%
      dplyr::filter(sim == "TD") %>%
      dplyr::select(log_lr) %>%
      stats::quantile(probs = 0.95, na.rm = TRUE)

    gg <- gg +
      # Background grey area
      ggplot2::geom_rect(ggplot2::aes(
        xmin = threshold_dd, xmax = threshold_td,
        ymin = 0, ymax = 3),
        fill = "grey85"
      ) +
      ggplot2::geom_rect(ggplot2::aes(
        xmin = -Inf, xmax = threshold_dd,
        ymin = 0, ymax = 3),
        fill = "#d0d8ff" # light blue
      ) +
      ggplot2::geom_rect(ggplot2::aes(
        xmin = threshold_td, xmax = Inf,
        ymin = 0, ymax = 3),
        fill = "#d2ecd5" # light green
      ) +
      # Plot thresholds
      ggplot2::geom_vline(xintercept = threshold_dd, color = "green4") +
      ggplot2::geom_vline(xintercept = threshold_td, color = "blue")

  } # end thresholds

  # Plot proper ------------------------------------------------------------
  gg <- gg + ggplot2::geom_density(
    ggplot2::aes(x = log_lr, fill = sim),
    position = "identity",
    na.rm = TRUE,
    alpha = 0.4
  )
  return(gg)
}
