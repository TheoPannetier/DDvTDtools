#' Get LTT tables and average LTT for a set of trees
#'
#' Calls [TreeSim::LTT.plot.gen()] to get LTT coordinates of trees in a
#' simulated data set, and the average LTT coordinates
#'
#' @inheritParams params_doc
#' @return a list of tibbles of the LTT coordinates.
#' The first element corresponds to the average LTT, and each other element
#' is to the LTT of a single tree.
#' @author Théo Pannetier
#' @export

get_ltt_tbls <- function(sim, para, with_extinct = FALSE) {
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
  phylos <- read_sim_multiPhylo(
    sim = sim,
    para = para,
    with_extinct = with_extinct
    )
  crown_age <- para_to_pars(para)[1]

  ltts <- TreeSim::LTT.plot.gen(trees = list(phylos))
  # First element is the average LTT, the rest are LTTs of each tree
  ltt_tbls <- lapply(ltts, function(ltt) {
    tibble::tibble(
      "time" = ltt[, 1],
      "N" = ltt[, 2],
      "sim" = factor(sim, levels = DDvTDtools::arg_sim())
    )
  })
  # Cure average LTT
  ltt_tbls[[1]] <- ltt_tbls[[1]] %>%
    # points from 0 to 2 tips (crown) are irrelevant
    dplyr::filter(N >= 2.00) %>%
    # Too many entries, we bin time by 0.01
    dplyr::mutate(time = plyr::round_any(time, 0.01)) %>%
    dplyr::group_by(time, sim) %>%
    dplyr::summarise(N = mean(N))
  ltt_tbls
}
