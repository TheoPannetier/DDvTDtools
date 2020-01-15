#' Fill missing results row with default values
#'
#' Searches a results data frame for missing rows and fills them with default
#' values.
#'
#' @inheritParams params_doc
#' @param results a results data frame, as produced by [run_optim()] or accessed
#' through [read_optim_results()].
#'
#' @author Théo Pannetier
#' @export

fill_missing_rows <- function(results, para) {
  assert_DDvTD_wd()
  assert_para(para)

  missing_rows <- which(!(1:1000 %in% results$mc))

  if (length(missing_rows) > 0) {
    sim <- results$sim %>% as.character() %>% unique()
    optim <- results$optim %>% as.character() %>% unique()
    pars <- c(results$crown_age[1], results$true_lambda0[1], results$true_mu0[1], results$true_K[1])
    if(any(para_to_pars(para) != pars)) {
      stop("para input does not match true parameters in metadata")
    }
    all_brts <- get_multi_brts(sim = sim, para = para)

    new_rows <- results_optim_struct()
    for(mc in missing_rows) {
      cat("Filling missing row:", mc, "\n")
      new_row <- format_optim_results_row(
        mc = mc,
        sim = sim,
        optim = optim,
        brts = all_brts[[mc]],
        true_pars = para_to_pars(para),
        init_pars = get_default_initpars(
          true_pars = para_to_pars(para),
          optim = optim,
          brts = all_brts[[mc]]
        ),
        ML_output = data.frame(
          lambda = NA, mu = NA, K = NA,
          loglik = -Inf, results = -1, conv = -1
        ),
        num_cycles = NA,
        methode = NA,
        optimmethod = NA,
        jobID = NA
      )
      results <- rbind(results, new_row)
    }
  } else {
    cat("No missing row for this set.\n")
  }
  results
}
