#' Get default initial parameter values for run_ML optimisation
#'
#' Internal function called only by \code{run_ML}.
#'
#' @author Théo Pannetier

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
