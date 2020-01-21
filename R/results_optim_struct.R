#' Structure of the optimisation results data frames
#'
#' Returns a skeleton for the data frame in which the output of [run_optim()]
#' is stored.
#'
#' @details The dataset contains the following variables:
#' * __sim__ factor, the name of the model used for simulation
#' * __ntips__ numeric, the number of tips in the tree
#' * __crown_age__ numeric, the crown age of the tree
#' * __true_lambda0__ numeric, the true value of \eqn{\lambda_0}
#' * __true_mu0__ numeric, the true value of \eqn{\mu_0}
#' * __true_K numeric__, the true value of K
#' * __mc__ numeric, the index of the tree (from 1 to 1000)
#' * __optim__ factor, the model fit to the tree
#' * __init_lambda0__ numeric, the initial value of \eqn{\lambda_0} used for
#' optimisation
#' * __init_mu0__, numeric, the initial value of \eqn{\mu_0} used for optimisation
#' * __init_k__ numeric, the initial value of K used for optimisation.
#' * __loglik__ numeric, the log-likelihood of the fitted model
#' * __AIC__ numeric, the AIC derived from the log-likelihood
#' * __lambda0_ML__ numeric, the maximum likelihood estimate of \eqn{\lambda_0}
#' * __mu0_ML__ numeric, the maximum likelihood estimate of \eqn{\mu_0}
#' * __K_ML__, the maximum likelihood estimate of K.
#' * __hasConverged__ logical, has the optimisation chain converged to a value?
#' * __numCycles__ numeric, number of cycles of optimisation, run in a loop. See
#' [DDD::dd_ML()]
#' * __methode__ factor, the method used to solve the likelihood equation. See
#' [DDD::dd_ML()]
#' * __optimmethod__ factor, the method used for optimisation (either simplex or
#' subplex). See [DDD::dd_ML()]
#' * __jobID__ numeric, the ID of the job is the optimisation was run on a
#' high-performance cluster.
#' * __cond__ numeric, a code specifying how the likelihood is conditioned.
#' See [DDD::dd_ML()]
#'
#' @author Théo Pannetier
#' @export

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

