#' Plot log-LR distribution for DD & TD trees
#'
#' Produces the log-LR distribution for all trees sharing the same parameter
#' set, as Fig. 3 in Pannetier et al. (2020)
#'
#' @inheritParams params_doc
#' @param plot_thresholds logical, should threshold bars be plotted with the
#' distribution?
#' @param label_p_success logical, should the proportion of successes be added
#' as a label on the plot?
#' @param full_range logical. If \code{TRUE}, the range of the x-axis is set to
#' include all points in the dataset. Otherwise, default is \code{c(-5, 10)}.
#'
#' @author Théo Pannetier
#'
#' @export

plot_lr <- function(para,
                    init_k = get_init_k()[which(names(get_init_k()) == para)],
                    plot_thresholds = TRUE,
                    label_p_success = TRUE,
                    full_range = FALSE) {
  assert_DDvTD_wd()
  assert_para(para)
  assert_init_k(init_k)
  if (!is.logical(plot_thresholds)) {
    stop("invalid input - plot_thresholds must be logical")
  }

  # Fetch and assemble data sets -----------------------------------------------
  lr_tbl <- lapply(arg_sim(), function(sim) {
    get_lr_tbl(sim = sim, para = para, init_k = init_k) %>%
      dplyr::mutate("sim" = sim)
  }) %>% dplyr::bind_rows()

  # Set up scope -----------------------------------------------------
  # if (full_range) {
  #   xlim <- c(min(-10, min(lr_tbl$lr)) , max(10, max(lr_tbl$lr)))
  # } else {
  #   xlim <- c(-5, 10)
  # }

  # Plot  ---------------------------------------------------------------
  gg <- ggplot2::ggplot(data = lr_tbl) +
    # data are plotted at the end to be overlaid on grey rect background
    ggplot2::scale_fill_manual(values = c("green4", "blue"), guide = FALSE) +
    ggplot2::coord_cartesian(
      # xlim = xlim,
      ylim = c(0, 1)
    ) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(
      title = make_plot_title_expr(para),
      x = latex2exp::TeX("logL_{DD} - logL_{TD}"),
      y = "Density"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 14))

  # Thresholds ---------------------------------------------------------
  if (plot_thresholds == TRUE) {
    threshold_dd <- lr_tbl %>%
      dplyr::filter(sim == "DD") %>%
      dplyr::select(lr) %>%
      stats::quantile(probs = 0.05, na.rm = TRUE)
    threshold_td <- lr_tbl %>%
      dplyr::filter(sim == "TD") %>%
      dplyr::select(lr) %>%
      stats::quantile(probs = 0.95, na.rm = TRUE)

    gg <- gg +
      # Background grey area
      ggplot2::geom_rect(ggplot2::aes(
        xmin = threshold_dd, xmax = threshold_td,
        ymin = 0, ymax = 1.5),
        fill = "grey85"
      ) +
      # Plot thresholds
      ggplot2::geom_vline(xintercept = threshold_dd, color = "green4") +
      ggplot2::geom_vline(xintercept = threshold_td, color = "blue")

    # Labels if requested as well ------------------------------------------
    if (label_p_success) {

      p_dd <- lr_tbl %>%
        dplyr::filter(sim == "DD") %>%
        dplyr::summarise(
          # mean of a condition is a proportion
          "p_dd"  = mean(lr > threshold_td, na.rm = TRUE) %>% round(3)
        )
      p_td <- lr_tbl %>%
        dplyr::filter(sim == "TD") %>%
        dplyr::summarise(
          "p_td"  = mean(lr < threshold_dd, na.rm = TRUE) %>% round(3)
        )

      gg <- gg +
        ggplot2::annotate(
          geom = "label",
          label = paste0("p_dd = ", p_dd),
          x = 6,
          y = 0.9,
          size = 3,
          label.r = ggplot2::unit(0.1, "lines"),
          label.padding = ggplot2::unit(0.5, "lines"),
          fill = "grey95",
          hjust = 0
        ) +
        ggplot2::annotate(
          geom = "label",
          label = paste0("p_td = ", p_td),
          x = -4,
          y = 0.9,
          size = 3,
          label.r = ggplot2::unit(0.1, "lines"),
          label.padding = ggplot2::unit(0.5, "lines"),
          fill = "grey95",
          hjust = 0
        )
    } # end labels
  } # end thresholds

  # Plot proper ------------------------------------------------------------
  gg <- gg + ggplot2::geom_density(
    ggplot2::aes(x = lr, fill = sim),
    position = "identity",
    na.rm = TRUE,
    alpha = 0.3
  )
  return(gg)
}
