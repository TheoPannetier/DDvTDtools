#' Computes the proportion of trees that can successfully be assigned to either
#' model
#'
#' Computes the proportion of trees in the dataset that reject the opposite
#' model, i.e. logLR is beyond the corresponding threshold computed by
#' [get_lr_threshold()].
#'
#' @inheritParams params_doc
#'
#' @author Theo Pannetier
#' @export

get_p_success <- function(sim, para, init_k) {
  lr_table <- get_lr(
    sim = sim,
    para = para,
    init_k = init_k
    )
  if (sim == "DD") {
    success_threshold <- get_lr_threshold(
      sim = "TD",
      para = para,
      init_k = init_k,
      percentile = 0.95
    )
    n_success <- length(which(lr_table > success_threshold))
    p_success <- round(n_success / length(lr_table), 3)

  } else if (sim == "TD") {
    success_threshold <- get_lr_threshold(
      sim = "DD",
      para = para,
      init_k = init_k,
      percentile = 0.05
    )
    n_success <- length(which(lr_table < success_threshold))
    p_success <- round(n_success / length(lr_table), 3)
  }
 p_success
}
