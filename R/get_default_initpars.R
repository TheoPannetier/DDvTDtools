#' Get default initial parameter values for run_optim optimisation
#'
#' Returns default initial parameter values used to fit \code{optim} models to
#' phylogenies with \code{run_optim()}. Called by \code{run_optim()}.
#'
#' @param true_pars numeric vector. The true parameter values used to simulate
#' the trees.
#' @inheritParams params_doc
#'
#' @details For the DD and TD models, initial parameter values are by default
#' set to the true values used to simulate the trees. For thr CR model, la0
#' is set to the expected speciation rate, knowing the age, number of tips and
#' the extinction rate for the tree. This is computed using equation (15) from
#' Kendall (1948) for the expected number of particles over time in a
#' constant-rate birth-death process.
#'
#' @references Kendall, David G. On the Generalized "Birth-and-Death"
#' Process. Ann. Math. Statist. 19 (1948), no. 1, 1--15.
#' doi:10.1214/aoms/1177730285
#'
#' @author Théo Pannetier
#'
#' @export

get_default_initpars <- function(true_pars, optim, brts){
  assert_optim(optim)
  if(!is.numeric(true_pars)){stop("true_pars should be numeric.")}
  if(!is.numeric(brts)){stop("brts should be numeric.")}

  N <- length(brts) + 1

  if (optim == "CR"){
    # Set a realistic estimate for lambda0 under CR
    exp_lambda0 <- (log(N * 0.5) + true_pars[3]) / true_pars[1]
    init_pars <-  c(exp_lambda0, true_pars[3])
  }
  else {
    init_pars <- true_pars[2:4]
  }
  return(init_pars)
}
