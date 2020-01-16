#' Assemble a table containing likelihood ratios
#'
#' Calls \code{get_lr} and binds results in a table with corresponding
#' \code{sim} ans \code{para} values.
#'
#' @inheritParams params_doc
#'
#' @author Theo Pannetier
#' @export
#'
get_lr_table <- function(para, init_k) {
  lr_table <- data.frame(
    lr = numeric(),
    para = factor(levels = rev(arg_para()), ordered = TRUE),
    sim = factor(levels = arg_sim(), ordered = TRUE)
  )

  for (sim in arg_sim()) {
    lr <- get_lr(sim = sim, para = para, init_k = init_k)
    subtable <- data.frame(
      lr = lr,
      para = factor(para, levels = rev(arg_para()), ordered = TRUE),
      sim = factor(sim, levels = arg_sim(), ordered = TRUE)
    )
    lr_table <- rbind(lr_table, subtable)
  }
  lr_table
}
