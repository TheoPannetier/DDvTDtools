# Return the average number of lineage over time for a dataset
#
#' @param sim name of the model used to simulate the trees. See \code{get_sim_names()}
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values.
#' @param with_extinct logical. \code{with_extinct = F} for the reconstructed
#' tree, \code{with_extinct = T} for the complete tree.
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
  avg_ltt_matrix <- TreeSim::LTT.plot.gen(trees = list(phylos))[[1]]
  avg_ltt <- dplyr::tibble(
    "Time" = as.numeric(avg_ltt_matrix[,1]),
    "avg_N" = as.numeric(avg_ltt_matrix[,2]),
    "sim" = factor(sim, levels = get_sim_names())
  )
  crown_n <- which(round(avg_ltt$Time, digits = 6) == -para_to_pars(para)[1])
  avg_ltt <- avg_ltt[-crown_n,]

  # Alternative with tidyverse (/!\ Results erroneous)
  # phylo_list <- DDvTDtools::get_sim_multiPhylo(sim, para)
  #
  # ltt_coords <- purrr::map(
  #   phylo_list,
  #   function (x) {make_tibble_ltt(x)}
  # ) %>%
  #   bind_rows() %>%
  #   mutate(time_round = plyr::round_any(time, 0.0001)) %>%
  #   group_by(time_round) %>%
  #   summarise(N_mean = mean(N), N_variance = var(N))
}
