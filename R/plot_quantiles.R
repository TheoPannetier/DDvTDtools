#' Add quantile bars to logLR distribution plot
#'
#' Extracts quantiles from the loglik ratios distribution and add them to the
#' plot
#'
#' @param df data frame containing the data, as passed from \code{plot_LR_DDvTD}
#' @param which_dd numeric, which quantiles to plot for dd.
#' @param which_td numeric, which quantiles to plot for td.

plot_quantiles <- function(df,
                           which_dd = c(0.05, 0.95),
                           which_td = c(0.05, 0.95)
                           ) {

  quantile_dd <- df %>%
    subset(sim == "DD", select = LR) %>%
    quantile(df$LR, probs = which_dd, na.rm = TRUE)
  quantile_dd <- ggplot2::geom_vline(
    xintercept = quantile_dd,
    color = "green4",
    alpha = 0.5
    )

  quantile_td <- df %>%
    subset(sim == "TD", select = LR) %>%
    quantile(df$LR, probs = which_td, na.rm = TRUE)
  quantile_td <- ggplot2::geom_vline(
    xintercept = quantile_td,
    color = "blue",
    alpha = 0.5
  )

  return(c(quantile_dd, quantile_td))
}
