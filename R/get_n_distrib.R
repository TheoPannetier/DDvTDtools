get_n_distrib <- function(sim, para) {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  
  phylo_list <- DDvTDtools::get_sim_multiPhylo(sim, para)
  
  n_table <- purrr::map(
    phylo_list,
    function (x) {make_tibble_ltt(x)}
  ) %>%
    bind_rows() %>% 
    filter(time == 0)
  n_table[,"N"]
}
