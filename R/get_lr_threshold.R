#' Get the relevant log LR threshold value to reject either model
#'
#' Computes the threshold value for the log-likelihood ratio to reject either
#' model.
#'
#' @param sim
#' @param para
#' @param init_k
#' @param percentile numeric value between 0 and 1, the percentile on which the
#' threshold should be based (normally 0.05 for DD, 0.95 for TD).
#'
#' @author Theo Pannetier
#' @export

get_lr_threshold <- function(sim, para, init_k, percentile) {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  assert_init_k(init_k)
  if (!dplyr::between(percentile, 0, 1)) {
    stop("prob must be a proportion")
  }

  lr <- get_lr(sim = sim, para = para, init_k = init_k)
  lr %>% stats::quantile(probs = percentile, na.rm = TRUE)
}
