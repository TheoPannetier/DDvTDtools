#' Assemble a table containing likelihood ratios
#'
#' Calls [get_lr_tbl()] for a set of simulations and returns the
#' log-likelihood ratio in a table.
#'
#' @inheritParams params_doc
#'
#' @author Theo Pannetier
#' @export
#'
get_lr_tbl <- function(sim,
                       para,
                       init_k = get_init_k()[which(names(get_init_k()) == para)]
                       ) {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  assert_init_k(init_k)

  lr_tbl <- lapply(
    c("DD", "TD"),
    function(optim) {
      read_optim_results(
        sim = sim, para = para, optim = optim, init_k = init_k
      ) %>%
        dplyr::select("mc", "optim", "loglik") %>%
        dplyr::mutate(
          # replace -Inf with NAs
          "loglik" = ifelse(loglik == -Inf, NA, loglik)
        )
    }) %>%
    dplyr::bind_rows() %>%
    # Pivot to get 1 column for DD loglik and 1 for TD loglik
    tidyr::pivot_wider(
      names_prefix = "loglik_",
      names_from = optim,
      values_from = loglik
    ) %>%
    dplyr::transmute(
      mc,
      "lr" = loglik_DD - loglik_TD
    )

  return(lr_tbl)
}
