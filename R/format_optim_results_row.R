#' Assemble a row of results for run_optim output
#'
#' Internal function called by \code{run_optim}. Formats optimisation results and
#' metadata in a data frame row.
#'
#' @inheritParams params_doc
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
#' @param cond numeric, conditionning parameter passed to
#' \code{dd_ML}/\code{bd_ML}. See \code{?dd_ML} for possible values.
#'
#' @author Théo Pannetier
#'
#' @export

format_optim_results_row <- function(mc, sim, optim, brts, true_pars, init_pars,
                             ML_output, num_cycles = 1, methode, optimmethod,
                             jobID, cond)
{
  df <- data.frame(
    sim = factor(sim, levels = arg_sim()),
    ntips = length(brts) + 1,
    crown_age = true_pars[1],
    true_lambda0 = true_pars[2],
    true_mu0 = true_pars[3],
    true_K = true_pars[4],
    mc = mc,
    optim = factor(optim, levels = arg_optim()),
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
    methode = factor(
      methode, levels = c("lsoda", "ode45", "lsodes", "analytical")
      ),
    optimmethod = factor(optimmethod, levels = c("simplex", "subplex")),
    jobID = jobID,
    cond = cond,
    row.names = mc
  )
  if (!is.na(df$K_ML) & df$K_ML == 0) {df$K_ML <- NA}
  return(df)
}
