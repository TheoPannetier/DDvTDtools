#' Read results of the model optimisation
#'
#' Reads a result dataframe produced by [run_optim()]. The file is expedcted to
#' be found in `DDvTD/data/optim/` and have the default name computed by
#' [run_optim()].
#'
#' @inheritParams params_doc
#'
#' @author Theo Pannetier
#' @export

read_optim_results <- function(sim, optim, para, init_k = "true_k") {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  assert_optim(optim)
  assert_init_k(init_k)

  res_filename <- paste0("sim", sim, "_optim", optim, "_", para, "_", init_k,".rds")

  if(!file.exists(paste0("data/optim/", res_filename))){
    stop(paste0("data/optim/",res_filename, " does not exist"))
  }

  res <- readRDS(paste0("data/optim/", res_filename)) %>%
    tibble::as_tibble()

  return(res)
}
