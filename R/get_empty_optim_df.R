#' Get an empty skeleton for run_ML output data frame.
#'
#' Internal function called only by \code{run_ML}.
#'
#' @author Théo Pannetier

get_empty_optim_df <- function(){
  data.frame(
    sim = factor(levels = get_sim_names()),
    ntips = numeric(),
    crown_age = numeric(),
    true_lambda0 = numeric(),
    true_mu0 = numeric(),
    true_K = numeric(),
    mc = numeric(),
    optim = factor(levels = get_optim_names()),
    init_lambda0 = numeric(),
    init_mu0 = numeric(),
    init_K = numeric(),
    loglik = numeric(),
    AIC = numeric(),
    lambda0_ML = numeric(),
    mu0_ML = numeric(),
    K_ML = numeric(),
    hasConverged = logical(),
    numCycles = numeric(),
    methode = factor(levels = c("lsoda", "ode45", "lsodes", "analytical")),
    optimmethod = factor(levels = c("simplex", "subplex")),
    jobID = numeric(),
    cond = numeric()
  )
}

