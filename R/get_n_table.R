#' Get N for each tree in a dataset
#'
#' Assemble a table containing the number of tips in each tree in a results data
#' set.
#'
#' @param sim
#' @param para
#'
#' @author Theo Pannetier
#' @export

get_n_table <- function(sim, para) {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)

  phylo_list <- DDvTDtools::get_sim_multiPhylo(sim, para)

  n_table <- purrr::map(
    phylo_list,
    function (x) {
      dplyr::tibble(
        time = ape::ltt.plot.coords(x)[,1],
        N = ape::ltt.plot.coords(x)[,2]
      )
    }
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(time == 0)
  n_table[,"N"]
}
