#' Get an empty data frame to initiate run_optim output.
#'
#' Internal function called only by \code{run_optim}.
#'
#' @author Théo Pannetier

results_optim_struct <- function(){
  data.frame(
    sim = factor(levels = arg_sim()),
    ntips = numeric(),
    crown_age = numeric(),
    true_lambda0 = numeric(),
    true_mu0 = numeric(),
    true_K = numeric(),
    mc = numeric(),
    optim = factor(levels = arg_optim()),
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

