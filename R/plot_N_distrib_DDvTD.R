#' Plot the distribution of N for DD & TD trees
#'
#' Jointly plots the distribution of the number of tips at present for a pair
#' of TD and DD trees that share the same parameter set.
#' @param para numeric or character. A four-digits code specifying a set of parameter values.
#'
#' @author Théo Pannetier
#' @export

plot_N_distrib_DDvTD <- function(para){
  assert_DDvTD_wd()
  assert_para(para)

  trees_DD <- get_sim_multiPhylo(sim = "DD", para = para)
  N_distrib <- data.frame(
    N = numeric(),
    sim = factor(levels = get_sim_names())
  )
  for(mc in seq_along(trees_DD)){
    row <- data.frame(
      N = ape::Ntip(trees_DD[[mc]]),
      sim = factor("DD", levels = get_sim_names())
    )
    N_distrib <- rbind(N_distrib, row)
  }

  trees_TD <- get_sim_multiPhylo(sim = "TD", para = para)
  for(mc in seq_along(trees_TD)){
    row <- data.frame(
      N = ape::Ntip(trees_TD[[mc]]),
      sim = factor("TD", levels = get_sim_names())
    )
    N_distrib <- rbind(N_distrib, row)
  }

  # boxplot(N_distrib$N ~ N_distrib$sim)

  gg <- ggplot2::ggplot(data = N_distrib, ggplot2::aes(x = sim, y = N, fill = sim)) +
    ggplot2::geom_violin() +
    #ggplot2::geom_boxplot() +
    ggplot2::geom_hline(yintercept = 40, color = "red", linetype = "dashed") +
    ggplot2::scale_fill_manual(values = c("green4", "blue"),  guide = FALSE) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank())

  gg
  # ggtitle("DAISIE extintion rate estimates in ontogeny \nand null-ontogeny scenarios")  +
  #   ylab("Extinction rate estimates (log transformed)") +
  #   xlab(element_blank()) +
  #   theme(
  #     axis.ticks.x = element_blank(),
  #     legend.position = "none",
  #     axis.line = element_line("black"),
  #     panel.background = element_blank()
  #   )
  # scale_x_discrete(labels=c("island_ontogeny" = "Island ontogeny", "no_ontogeny" = "Null-ontogeny")) +
  #   geom_errorbar( # Add horizontal bars in set postion (specify in hlines_df)
  #     data = hlines_df,
  #     aes(y = NULL, ymax = hline, ymin = hline),
  #     color = "orange2", size = 1.1
  #   )
}



