#' Compute likelihood ratios for a results dataset
#'
#' @param sim character. The name of a simulation model
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values. See \code{get_para_values()} for possible values.
#' @param init_k character, the setting used to initialise parameter K.
#' See \code{get_possible_init_k()} for possible values.
#'
#' @author Théo Pannetier
#' @export

get_lr <- function(sim, para, init_k = "true_k") {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  assert_init_k(init_k)

  res_DD <- read_optim_table(
    sim = sim, para = para, optim = "DD", init_k = init_k
    )
  res_TD <- read_optim_table(
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
