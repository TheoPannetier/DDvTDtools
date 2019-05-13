#' Fill missing results row with default values
#'
#' Function used to fill result data for trees that miss any result.
#' Fills the result matrix for a parameter set with default values for these
#' missing trees
#'
#' @param sim character. The model used to simulate the tree.
#' @param optim character. The model fitted to the tree.
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values. See \code{get_para_values()} for possible values.
#'
#' @author Théo Pannetier
#' @export

fill_missing_rows <- function(sim, optim, para) {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_optim(optim)
  assert_para(para)

  res <- read_optim_table(sim = sim, optim = optim, para = para)
  missing_rows <- which(!(1:1000 %in% res$mc))

  if(length(missing_rows) > 0) {
    cat("Filling missing rows:", missing_rows, "\n")
    new_rows <- get_empty_optim_df()
    for(mc in missing_rows) {
      new_row <- get_optim_df_row(
        mc = mc,
        sim = sim,
        optim = optim,
        brts = get_brts(sim = sim, para = para, mc = mc),
        true_pars = para_to_pars(para),
        init_pars = get_default_initpars(
          true_pars = para_to_pars(para),
          optim = optim,
          brts = get_brts(sim = sim, para = para, mc = mc)
          ),
        ML_output = data.frame(
          lambda = NA, mu = NA, K = NA,
          loglik = -Inf, df = -1, conv = -1
          ),
        num_cycles = NA,
        methode = NA,
        optimmethod = NA,
        jobID = NA
      )
      res <- rbind(res, new_row)
    }
  } else {
    cat("No missing row for this set.\n")
  }
  res
}