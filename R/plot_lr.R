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
#' @param which_geom character giving the type of plot to be drawn. Options are
#' \code{"density"} and \code{"histograms"}.
#'
#' @author Théo Pannetier
#'
#' @export

plot_lr <- function(para,
                    init_k = "true_k",
                    plot_thresholds = TRUE,
                    label_p_success = TRUE,
                    full_range = FALSE,
                    which_geom = "density") {
  assert_DDvTD_wd()
  assert_para(para)
  assert_init_k(init_k)
  if (!is.logical(plot_thresholds)) {
    stop("invalid input - plot_thresholds must be logical")
  }
  if (!which_geom %in% c("density", "histogram")) {
    stop("invalid input - which_geom options are 'density' or 'histogram'")
  }
  # Fetch and assemble data sets -----------------------------------------------
  lr_table <- get_lr_table(para = para, init_k = init_k)

  # Set up label variables -----------------------------------------------------
  ymax <- ifelse(which_geom == "histogram", 500, 1)
  if (full_range) {
    xlim <- c(min(-10, min(lr_table$lr)) , max(10, max(lr_table$lr)))
  } else {
    xlim <- c(-5, 10)
  }

  # Plot, proper ---------------------------------------------------------------
  gg <- ggplot2::ggplot(data = lr_table) +
    ggplot2::scale_fill_manual(values = c("green4", "blue"), guide = FALSE) +
    ggplot2::coord_cartesian(xlim = xlim,
                             ylim = c(0, ymax)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(
      title = make_plot_title_expr(para),
      x = latex2exp::TeX("logL_{DD} - logL_{TD}"),
      y = "Density"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 14))
  if (plot_thresholds == TRUE) {
    threshold_dd <- 0.05
    threshold_td <- 0.95
    x_bar_dd <- get_lr_threshold(
      sim = "DD",
      para = para,
      init_k = init_k,
      percentile = threshold_dd
    )
    x_bar_td <- get_lr_threshold(
      sim = "TD",
      para = para,
      init_k = init_k,
      percentile = threshold_td
    )
    gg <- gg +
      ggplot2::geom_rect(ggplot2::aes(
        xmin = x_bar_dd,
        xmax = x_bar_td,
        ymin = 0,
        ymax = 1.5
      ),
      fill = "grey85") +
      ggplot2::geom_vline(xintercept = x_bar_dd, color = "green4") +
      ggplot2::geom_vline(xintercept = x_bar_td, color = "blue")

  }
  if (which_geom == "density") {
    gg <- gg +  ggplot2::geom_density(
      ggplot2::aes(x = lr, fill = sim),
      alpha = 0.2
    )
  } else {
    gg <- gg + ggplot2::geom_histogram(
      ggplot2::aes(x = lr, fill = sim),
      position = "dodge",
      binwidth = 0.5
    )
  }
  if (label_p_success) {
    p_dd <- get_p_success(sim = "DD",
                          para = para,
                          init_k = init_k)
    p_td <- get_p_success(sim = "TD",
                          para = para,
                          init_k = init_k)
    gg <- gg + ggplot2::annotate(
      geom = "label",
      label = paste0("p_DD = ", p_dd),
      x = 6,
      y = 0.9,
      size = 3,
      label.r = ggplot2::unit(0.1, "lines"),
      label.padding = ggplot2::unit(0.5, "lines"),
      fill = "grey95",
      hjust = 0
    ) + ggplot2::annotate(
      geom = "label",
      label = paste0("p_TD = ", p_td),
      x = -4,
      y = 0.9,
      size = 3,
      label.r = ggplot2::unit(0.1, "lines"),
      label.padding = ggplot2::unit(0.5, "lines"),
      fill = "grey95",
      hjust = 0
    )

  }
  gg
}
