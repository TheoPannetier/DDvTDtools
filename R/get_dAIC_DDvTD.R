#' Compute AIC_DD - AIC_TD for a set of trees
#'
#' @inheritParams params_doc
#'
#' @author Théo Pannetier
#' @export

get_dAIC_DDvTD <- function(sim, para){
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)

  res_DD <- read_optim_table(sim = sim, para = para, optim = "DD")
  res_TD <- read_optim_table(sim = sim, para = para, optim = "TD")

  dAIC <- c()
  for(mc in 1:1000){
    if (any(c(res_DD$AIC[mc], res_TD$AIC[mc]) %in% c(Inf, -Inf, NA, -1) )){
      dAIC <- c(dAIC, NA)
    } else {
      dAIC <-  c(dAIC, res_DD$AIC[mc] - res_TD$AIC[mc])
    }
  }
  dAIC
}
