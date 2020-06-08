#' Read a pair of `optim` tables and join them together
#' 
#' For a given combination of `sim` and `para`, this function will fetch the
#' two corresponding optimisation results tables; filter out the metadata
#' and bind them together. Also computes the log likelihood ratio along the way.
#' 
#' @inheritParams params_doc
#' @author Théo Pannetier
#' @export
join_optim_tbls <- function(sim, 
                            para,
                            init_k = get_init_k()[which(names(get_init_k()) == para)]
) {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  assert_init_k(init_k)
  
  optim_ls <- purrr::map(
    c("DD", "TD"),
    function(optim) {
      read_optim_results(
        sim = sim, para = para, optim = optim, init_k = init_k
      ) %>%
        dplyr::mutate(
          # replace -Inf with NAs
          "loglik" = ifelse(loglik == -Inf, NA, loglik)
        ) %>% 
        dplyr::select(mc, loglik, contains("init_"), contains("_ML")) %>% 
        dplyr::rename_with(tolower)
    })
  
  optim_tbl <- dplyr::full_join(
    optim_ls[[1]], optim_ls[[2]],
    by = c("mc"),
    suffix = c("_dd", "_td")
    ) %>% 
    dplyr::mutate("log_lr" = loglik_dd - loglik_td) %>% 
    dplyr::relocate(loglik_td, log_lr, .after = loglik_dd)
  
  return(optim_tbl)
}
