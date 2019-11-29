#' Compute the proportion of trees for which DD is preferred
#'
#' Returns p_DD, the proportion of trees in a parameter setting for which DD
#' fits better than TD (i.e. log-likelihood ratio > 0)
#'
#' @param sim character. The name of a simulation model
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values. See \code{get_para_values()} for possible values.
#' @param init_k character, the setting used to initialise parameter K.
#' See \code{get_possible_init_k()} for possible values.
#'
#' @author Théo Pannetier
#' @export

get_p_DD <- function(sim, para, init_k = "true_k") {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  assert_init_k(init_k)

  lr <- get_lr_table(sim = sim, para = para, init_k = init_k)
  length(which(lr > 0)) / 1000
}
