get_lr_threshold <- function(sim, para, init_k, prob) {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  assert_init_k(init_k)
  if (!dplyr::between(prob, 0, 1)) {
    stop("prob must be a proportion")
  }

  lr <- get_lr_table(sim = sim, para = para, init_k = init_k)
  lr %>% stats::quantile(probs = prob, na.rm = TRUE)
}
