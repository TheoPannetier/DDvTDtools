#' Compute logL_DD - logL_TD for a set of trees
#'
#' @param sim character. The name of a simulation model
#' @param para numeric or character. A four-digits code specifying a set of parameter values. See \code{get_para_values()} for possible values.
#' @author Théo Pannetier
#' @export

get_LR_DDvTD <- function(sim, para){
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)

  res_DD <- read_optim_table(sim = sim, para = para, optim = "DD")
  res_TD <- read_optim_table(sim = sim, para = para, optim = "TD")

  LR <- c()
  for(mc in 1:1000){
    if (any(c(res_DD$loglik[mc], res_TD$loglik[mc]) %in% c(Inf, -Inf, NA, -1) )){
      LR <- c(LR, NA)
    } else {
      LR <-  c(LR, res_TD$loglik[mc] - res_DD$loglik[mc])
    }
  }
  LR
}
