#' Format run_ML optimisation results in a data frame row
#'
#' Internal function called by \code{run_ML}. Formats optimisation results and
#' metadata in a data frame row.
#'
#' @param mc numeric, the index of the tree on which model \code{optim} was
#' fitted.
#' @param sim character. The model used to simulate the tree.
#' Call \code{get_sim_names} for possible values.
#' @param optim character. The model fitted to the tree.
#' Call \code{get_optim_names} for possible values.
#' @param brts numeric vector. Branching times of the tree, used to compute
#'  metadata entry \code{ntips}.
#' @param true_pars numeric vector. The true parameter values used to simulate
#' the tree.
#' @param init_pars numeric vector. The initial parameter values used for
#' likelihood optimisation.
#' @param ML_output numeric vector. Raw output of \code{DDD::dd_loglik} /
#' \code{DDD::bd_loglik}.
#' @param num_cycles numeric. Maximum number of optimisation cycles.
#' @param methode character. Integration methode for computing the likelihood.
#' See \code{DDD::dd_loglik} / \code{DDD::bd_loglik} for details.
#' @param optimmethod character. Optimisation methode used to compute the
#' maximum likelihood. See \code{DDD::dd_ML} / \code{DDD::bd_ML} for details.
#' @param jobID the ID number of the cluster job that returned the results.
#' Saved in metadata to retrieve logs, for example. A value of \code{NULL} means
#' that the result was obtained locally.
#'
#' @author Théo Pannetier
#'
#' @export

get_optim_df_row <- function(mc, sim, optim, brts, true_pars, init_pars, ML_output,
                             num_cycles = 1, methode, optimmethod, jobID)
{
  df <- data.frame(
    sim = factor(sim, levels = get_sim_names()),
    ntips = length(brts) + 1,
    crown_age = true_pars[1],
    true_lambda0 = true_pars[2],
    true_mu0 = true_pars[3],
    true_K = true_pars[4],
    mc = mc,
    optim = factor(optim, levels = get_optim_names()),
    init_lambda0 = init_pars[1],
    init_mu0 = init_pars[2],
    init_K = init_pars[3],
    loglik = ML_output$loglik,
    AIC = 2 * ML_output$df - 2 * ML_output$loglik,
    lambda0_ML = ML_output[,1],
    mu0_ML = ML_output[,2],
    K_ML = ML_output[,3],
    hasConverged = (ML_output$conv == 0),
    numCycles = num_cycles,
    methode = factor(methode, levels = c("lsoda", "ode45", "lsodes", "analytical")),
    optimmethod = factor(optimmethod, levels = c("simplex", "subplex")),
    jobID = jobID,
    row.names = mc
  )
  if(!is.na(df$K_ML) & df$K_ML == 0){df$K_ML <- NA}
  return(df)
}
