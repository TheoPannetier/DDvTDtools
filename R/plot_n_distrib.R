#' Plot the distribution of N for DD & TD trees as violin plots
#'
#' Plots the distribution of the number of tips at present for a pair of sets
#' of diversity-dependent and time-dependent simulated trees.
#'
#' @inheritParams params_doc
#'
#' @author Théo Pannetier
#' @export
#'
plot_n_distrib <- function(para) {
  assert_DDvTD_wd()
  assert_para(para)

  phylos <- list(
    "DD" = read_sim_multiPhylo("DD", para),
    "TD" = read_sim_multiPhylo("TD", para)
  )

  n_tbl <- purrr::map_dfc(
    phylos,
    function(x) {
      return("n" = ape::Ntip(x))
    }
  ) %>%
    tidyr::pivot_longer(
      everything(),
      names_to = "sim",
      values_to = "n"
    )

  n_plot <- n_tbl %>% ggplot2::ggplot(
    ggplot2::aes(x = sim, y = n, fill = sim)
    ) +
    ggplot2::geom_violin(scale = "width") +
    ggplot2::scale_fill_manual(values = c("green4", "blue"), guide = FALSE) +
    ggplot2::geom_hline(
      yintercept = para_to_pars(para)[4], color = "grey50", linetype = "dashed"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank()
    )

  return(n_plot)
}
