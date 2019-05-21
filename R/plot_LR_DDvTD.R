#' Plot logLR distribution for DD & TD trees
#'
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values. See \code{get_para_values()} for possible values.
#' @param init_k character, the setting used to initialise parameter K.
#' See \code{get_possible_init_k()} for possible values.
#' @param which_geom character giving the type of plot to be drawn. Options are
#' \code{"density"} and \code{"histograms"}.
#'
#' @author Théo Pannetier
#'
#' @export

plot_LR_DDvTD <- function(para, init_k = "true_k", which_geom = "density"){
  assert_DDvTD_wd()
  assert_para(para)
  assert_init_k(init_k)
  if(!which_geom %in% c("density", "histograms")) {
    stop("invalid input - which_geom options are 'density' or 'histogram'")
  }

  LR_table <- data.frame(
    LR = numeric(),
    para = factor(levels = rev(get_para_values()), ordered = TRUE),
    sim = factor(levels = get_sim_names(), ordered = TRUE)
  )

  for(sim in get_sim_names()){

    LR <- get_LR_DDvTD(sim = sim, para = para, init_k = init_k)
    LR_subtable <- data.frame(
      LR = LR,
      para = factor(para, levels = rev(get_para_values()), ordered = TRUE),
      sim = factor(sim, levels = get_sim_names(), ordered = TRUE)
    )
    LR_table <- rbind(LR_table, LR_subtable)
  }

  title = bquote(
    "Age"       ~ "=" ~ .(DDvTDtools::para_to_pars(para)[1]) ~ " " ~
      lambda[0]   ~ "=" ~ .(DDvTDtools::para_to_pars(para)[2]) ~ " " ~
      mu[0]       ~ "=" ~ .(DDvTDtools::para_to_pars(para)[3])
  )

  gg <- ggplot2::ggplot(
    data = LR_table
  ) +
    ggplot2::scale_fill_manual(values = c("green4", "blue"), guide = FALSE) +
    ggplot2::xlim(min(-5, min(LR)) , max(10, max(LR))) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = title,
      x = "loglik(DD/TD)"
    )
  if (which_geom == "density") {
    gg <- gg +  ggplot2::geom_density(
      ggplot2::aes(x = LR, fill = sim),
      alpha = 0.2
    )
  } else {
    gg <- gg + ggplot2::geom_histogram(
      data = subset(LR_table, sim == "DD"),
      ggplot2::aes(x = LR, fill = sim),
      alpha = 0.2,
      binwidth = 1
    ) +
      ggplot2::geom_histogram(
        data = subset(LR_table, sim == "TD"),
        ggplot2::aes(x = LR, fill = sim),
        alpha = 0.2,
        binwidth = 1
      )
  }
  gg
}
