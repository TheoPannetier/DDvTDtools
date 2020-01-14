#' Fill missing results row with default values
#'
#' Function used to fill result data for trees that miss any result.
#' Fills the result matrix for a parameter set with default values for these
#' missing trees
#'
#' @inheritParams params_doc
#' @param df a results data frame, i.e. as read by
#'  \code{DDvTDtools::read_optim_table()}
#'
#'
#' @author Théo Pannetier
#' @export

fill_missing_rows <- function(df, para) {
  assert_DDvTD_wd()
  assert_para(para)

  missing_rows <- which(!(1:1000 %in% df$mc))

  if (length(missing_rows) > 0) {
    sim <- df$sim %>% as.character() %>% unique()
    optim <- df$optim %>% as.character() %>% unique()
    pars <- c(df$crown_age[1], df$true_lambda0[1], df$true_mu0[1], df$true_K[1])
    if(any(para_to_pars(para) != pars)) {
      stop("para input does not match true parameters in metadata")
    }
    all_brts <- get_multi_brts(sim = sim, para = para)

    new_rows <- get_empty_optim_df()
    for(mc in missing_rows) {
      cat("Filling missing row:", mc, "\n")
      new_row <- get_optim_df_row(
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
          loglik = -Inf, df = -1, conv = -1
        ),
        num_cycles = NA,
        methode = NA,
        optimmethod = NA,
        jobID = NA
      )
      df <- rbind(df, new_row)
    }
  } else {
    cat("No missing row for this set.\n")
  }
  df
}
