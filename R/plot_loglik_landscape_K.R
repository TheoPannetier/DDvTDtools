#' Plot likelihood landscapes along K
#'
#' Plot likelihood curves of DD and TD models over parameter K from saved
#' results.
#'
#' @inheritParams params_doc
#'
#' @author Théo Pannetier
#' @export

plot_loglik_landscape_K <- function(sim, para, mc) {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  if(!is.numeric(mc)) {
    stop("mc must be a numeric")
  }

  landscape_data <- assemble_landscape_data(sim = sim, para = para, mc = mc)
  true_K <- para_to_pars(para)[4]
  N <- length(get_brts(sim = sim, para = para, mc = mc)) + 1
  max_lik <- max(landscape_data$loglik, na.rm = TRUE)
  k_ml <- landscape_data$K[order(landscape_data$loglik, decreasing = TRUE)[1]]

  x <- ggplot2::ggplot(
    data = landscape_data,
    ggplot2::aes(
      x = landscape_data$K,
      y = landscape_data$loglik
    )
  ) + ggplot2::geom_line(
    ggplot2::aes(
      colour = landscape_data$optim,
      linetype = landscape_data$fixed_pars
    )
  ) +
    ggplot2::geom_vline(
      xintercept = true_K,
      linetype = "longdash",
      color = "grey70"
    ) +
    ggplot2::geom_vline(
      xintercept = N,
      #linetype = "longdash",
      color = "grey",
      alpha = 0.8
    ) +
    ggplot2::labs(
      x = "K",
      y = "logL",
      title = paste(sim, "tree", mc)
    ) +
    ggplot2::scale_colour_manual(
      name = "Model",
      values = c("green4", "blue"),
      labels = c("DD", "TD")
    ) +
    ggplot2::scale_linetype_discrete(
      name  = "Fixed parameter values",
      labels = c("True values", "Estimated values")
    ) +
    ggplot2::coord_cartesian(
      xlim = c(k_ml - 10, k_ml + 10),
      ylim = c(max_lik - 20, max_lik + 20)
    )
  x
}
