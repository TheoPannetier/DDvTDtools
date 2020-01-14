#' Computes the average number of lineages over time for a set of phylogenies
#'
#' Returns the average number of lineage over time for a dataset of 1000 simulated
#' phylogenies
#'
#' @param sim name of the model used to simulate the trees.
#' See \code{get_sim_names()}
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values.
#' @param with_extinct logical. \code{with_extinct = FALSE} for the
#' reconstructed tree, \code{with_extinct = TRUE} for the complete tree.
#'
#' @author Théo Pannetier
#' @export

get_avg_ltt_table <- function(sim, para, with_extinct = FALSE) {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  if(!is.logical(with_extinct)) {
    stop('with_extinct must be a logical')
  }
  if (!requireNamespace("TreeSim", quietly = TRUE)) {
    stop("Package TreeSim is required for this function",
         call. = FALSE)
  }

  phylos <- get_sim_multiPhylo(
    sim = sim,
    para = para,
    with_extinct = with_extinct
    )
  crown_age <- para_to_pars(para)[1]

  avg_ltt_matrix <- TreeSim::LTT.plot.gen(trees = list(phylos))[[1]]
  avg_ltt <- dplyr::tibble(
    "raw_time" = as.numeric(avg_ltt_matrix[,1]),
    "raw_avg_n" = as.numeric(avg_ltt_matrix[,2]),
    "sim" = factor(sim, levels = get_sim_names())
  ) %>%
    # points from 0 to 2 tips (crown) are irrelevant
    dplyr::filter(round(raw_time, digits = 6) != -crown_age) %>%
    dplyr::bind_rows() %>%
    # needs to lighten the data frame by binning points
    dplyr::mutate(time = plyr::round_any(raw_time, 0.01)) %>%
    dplyr::group_by(time, sim) %>%
    dplyr::summarise(avg_n = mean(raw_avg_n))
  avg_ltt
}
