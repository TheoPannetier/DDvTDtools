#' Plot dAIC distribution for a set of parameter sets
#'
#' Plot the distribution of AIC_TD - AIC_DD for a given \code{sim} and a set of \code{para}.
#'
#' @param sim character. The name of a simulation model
#' @param para_set numeric or character vector. A set of four-digits codes specifying a set of parameter values.
#'
#' @author Théo Pannetier
#' @export
#'
plot_dAIC_para <- function(sim, para_set){
  assert_DDvTD_wd()
  assert_sim(sim)
  for(para in para_set){
    assert_para(para = para)
  }

  dAIC_table <- data.frame(
    dAIC = numeric(),
    para = factor(levels = rev(get_para_values()), ordered = TRUE),
    sim = factor(levels = get_sim_names(), ordered = TRUE)
  )

  for(para in para_set){

    dAIC <- get_dAIC_DDvTD(sim = sim, para = para)
    dAIC_subtable <- data.frame(
      dAIC = dAIC,
      para = factor(para,levels = rev(get_para_values()), ordered = TRUE),
      sim = factor(sim, levels = get_sim_names(), ordered = TRUE)
    )
    dAIC_table <- rbind(dAIC_table, dAIC_subtable)

  }

  sim_color <- ifelse(sim == "DD", "green4", "blue")

  gg <- ggplot2::ggplot(
    data = dAIC_table,
    ggplot2::aes(
      x = dAIC_table$para, y = dAIC_table$dAIC, fill = dAIC_table$sim)
  ) +
    ggplot2::geom_boxplot(
      outlier.colour = "black", outlier.shape = 16, outlier.size = 2,
      notch = FALSE
    )  +
    ggplot2::scale_fill_manual(values = sim_color, guide = FALSE) +
    ggplot2::ylim(-8,8) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::coord_flip()
  gg

}
