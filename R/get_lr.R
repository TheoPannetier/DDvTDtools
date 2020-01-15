#' Compute likelihood ratios for a results dataset
#'
#' @inheritParams params_doc
#'
#' @author Théo Pannetier
#' @export

get_lr <- function(sim, para, init_k = "true_k") {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  assert_init_k(init_k)

  res_DD <- read_optim_results(
    sim = sim, para = para, optim = "DD", init_k = init_k
    )
  res_TD <- read_optim_results(
    sim = sim, para = para, optim = "TD", init_k = init_k
    )

  lr <- c()
  for(mc in 1:1000) {
    if (any(
        c(res_DD$loglik[mc], res_TD$loglik[mc]) %in% c(Inf, -Inf, NA, -1))
      ) {
      lr <- c(lr, NA)
    } else {
      lr <-  c(lr, res_DD$loglik[mc] - res_TD$loglik[mc])
    }
  }
  lr
}
