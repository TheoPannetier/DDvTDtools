#' Plot likelihood landscapes along K
#'
#' Plot likelihood curves of DD and TD models over parameter K from saved
#' results.
#'
#' @param sim character, the name of a simulation model. See
#' \code{get_sim_names()} for possible values.
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values.
#' @param mc numeric, the index of the tree to get the landscape for.
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

    x <- ggplot2::ggplot(
      data = landscape_data,
      ggplot2::aes(
        x = K,
        y = loglik
      )
    ) + ggplot2::geom_line(
      ggplot2::aes(
        colour = optim,
        linetype = fixed_pars
      )
    ) +
      ggplot2::scale_colour_manual(
        values = c("green4", "blue")
      ) +
      ggplot2::geom_vline(
        xintercept = true_K,
        linetype = "longdash",
        color = "grey70"
      ) +
      ggplot2::geom_vline(
        xintercept = N,
        linetype = "longdash",
        color = "purple"
      ) +
      ggplot2::labs(
        x = "K",
        y = "logL",
        title = paste(sim, "tree", mc)
      )
    x
}


