#' Computes the AIC difference between DD and TD
#'
#' Reads a pair of `optim = "DD"` and `optim = "TD"` results data frames and
#' computes the AIC difference between them.
#'
#' @inheritParams params_doc
#'
#' @author Théo Pannetier
#' @export

get_delta_aic <- function(sim, para, init_k = "true_k"){
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)

  res_DD <- read_optim_results(
    sim = sim, para = para, optim = "DD", init_k = init_k
    )
  res_TD <- read_optim_results(
    sim = sim, para = para, optim = "TD", init_k = init_k
    )

  dAIC <- c()
  for (mc in 1:1000) {
    if (any(
      c(res_DD$AIC[mc], res_TD$AIC[mc]) %in% c(Inf, -Inf, NA, -1))
      ) {
      dAIC <- c(dAIC, NA)
    } else {
      dAIC <- c(dAIC, res_DD$AIC[mc] - res_TD$AIC[mc])
    }
  }
  dAIC
}
