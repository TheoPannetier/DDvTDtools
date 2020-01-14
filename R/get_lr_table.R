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
    para = factor(levels = rev(get_para_values()), ordered = TRUE),
    sim = factor(levels = get_sim_names(), ordered = TRUE)
  )

  for (sim in get_sim_names()) {
    lr <- get_lr(sim = sim, para = para, init_k = init_k)
    subtable <- data.frame(
      lr = lr,
      para = factor(para, levels = rev(get_para_values()), ordered = TRUE),
      sim = factor(sim, levels = get_sim_names(), ordered = TRUE)
    )
    lr_table <- rbind(lr_table, subtable)
  }
  lr_table
}
