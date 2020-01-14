#' Plot logLR distribution for DD & TD trees
#'
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values. See \code{get_para_values()} for possible values.
#' @param init_k character, the setting used to initialise parameter K.
#' See \code{get_possible_init_k()} for possible values.
#' @param plot_thresholds logical, should quantile bars be plotted with the
#' distribution?
#' @param label_p_success logical, should the proportion of successes be added as
#' a label on the plot?
#' @param threshold_dd numeric, quantiles of the lr distribution that should be
#' added to the plot.
#' @param threshold_td numeric, quantiles of the lr distribution that should be
#' added to the plot.
#' @param full_range logical. If \code{TRUE}, the range of the x-axis is set to
#' include all points in the dataset. Otherwise, the default is
#' \code{c(-10, 10)}.
#' @param which_geom character giving the type of plot to be drawn. Options are
#' \code{"density"} and \code{"histograms"}.
#'
#' @author Théo Pannetier
#'
#' @export

plot_blrt <- function(para,
                          init_k = "true_k",
                          plot_thresholds = TRUE,
                          label_p_success = FALSE,
                          threshold_dd = 0.05,
                          threshold_td = 0.95,
                          full_range = FALSE,
                          which_geom = "density"){
  assert_DDvTD_wd()
  assert_para(para)
  assert_init_k(init_k)
  if (!is.logical(plot_thresholds)) {
    stop("invalid input - add_quantiles must be logical")
  }
  if (!which_geom %in% c("density", "histogram")) {
    stop("invalid input - which_geom options are 'density' or 'histogram'")
  }
  if (!is.numeric(threshold_dd) | !is.numeric(threshold_td)) {
    stop("invalid input - quantiles_dd and quantiles_td must be numeric")
  }

  # Fetch and assemble data sets -----------------------------------------------
  lr_table <- get_lr_table(para = para, init_k = init_k)

  # Set up label variables -----------------------------------------------------
  title <-  bquote(
   "Age"     ~ "=" ~ .(DDvTDtools::para_to_pars(para)[1]) ~ " " ~
   lambda[0] ~ "=" ~ .(DDvTDtools::para_to_pars(para)[2]) ~ " " ~
   mu[0]     ~ "=" ~ .(DDvTDtools::para_to_pars(para)[3])
  )
  ymax <- ifelse(which_geom == "histogram", 500, 1)
  if (full_range) {
    xlim <- c(min(-10, min(lr_table$lr)) , max(10, max(lr_table$lr)))
  } else {
    xlim <- c(-5, 10)
  }

  # Plot, proper ---------------------------------------------------------------
  gg <- ggplot2::ggplot(
    data = lr_table
  ) +
    ggplot2::scale_fill_manual(values = c("green4", "blue"), guide = FALSE) +
    ggplot2::coord_cartesian(
      xlim = xlim,
      ylim = c(0, ymax)
      ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, 0.2)
    ) +
    ggplot2::labs(
      x = latex2exp::TeX("logL_{DD} - logL_{TD}"),
      y = "Density"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 14))
  if (plot_thresholds == TRUE) {
    x_bar_dd <- get_lr_threshold(
      sim = "DD", para = para, init_k = init_k,  percentile = threshold_dd
        )
    x_bar_td <- get_lr_threshold(
      sim = "TD", para = para, init_k = init_k,  percentile = threshold_td
    )
    gg <- gg +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = x_bar_dd, xmax = x_bar_td, ymin = 0, ymax = 1.5),
        fill = "grey85"
      ) +
      ggplot2::geom_vline(xintercept = x_bar_dd, color = "green4") +
      ggplot2::geom_vline(xintercept = x_bar_td, color = "blue")

  }
  if (which_geom == "density") {
    gg <- gg +  ggplot2::geom_density(
      ggplot2::aes(x = lr_table$lr, fill = lr_table$sim),
      alpha = 0.2 #,
      #bw = 0.15
    )
  } else {
    gg <- gg + ggplot2::geom_histogram(
      ggplot2::aes(x = lr_table$lr, fill = lr_table$sim),
      position = "dodge",
      binwidth = 0.5
    )
  }
  if (label_p_success) {
    p_dd <- get_p_success(sim = "DD", para = para, init_k = init_k)
    p_td <- get_p_success(sim = "TD", para = para, init_k = init_k)
    gg <- gg + ggplot2::annotate(
      geom = "label",
      label = paste0("      = ", p_dd),
      # label = bquote(mu[DD] ~ "=" ~ .(p_dd)),
      x = 6, y = 0.9,
      size = 3,
      label.r = ggplot2::unit(0.1, "lines"),
      label.padding = ggplot2::unit(0.5, "lines"),
      fill = "grey95",
      hjust = 0
    ) + ggplot2::annotate(
      geom = "label",
      label = paste0("      = ", p_td),
      x = -4, y = 0.9,
      size = 3,
      label.r = ggplot2::unit(0.1, "lines"),
      label.padding = ggplot2::unit(0.5, "lines"),
      fill = "grey95",
      hjust = 0
    )

  }
  gg
}
