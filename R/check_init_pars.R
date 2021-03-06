#' Check initial parameter values for run_optim
#'
#' Checks that default or user-specified values entered to initialize the
#' ML optimisation do not break forbidden conditions inside \code{DDD::dd_ML()}
#' and \code{DDD::bd_ML()}.
#'
#' @inheritParams params_doc
#' @param init_pars numeric vector. The initial parameter values to be checked.
#'
#'
#'
#' @author Théo Pannetier
#'
#' @export

check_init_pars <- function(init_pars, optim, brts){
  assert_optim(optim)
  if(!is.numeric(init_pars)){stop("brts should be a numeric vector.")}
  if(!is.numeric(brts)){stop("init_pars should be a numeric vector.")}
  if(length(init_pars) != 3){
    stop("init_pars should have length 3")
  }

  if(init_pars[1] < 0){ stop("Initial lambda0 has a negative value.") }
  if(init_pars[2] < 0){ stop("Initial mu0 has a negative value.") }
  if(init_pars[1] <= init_pars[2]){
    init_pars[1] <- init_pars[2] * 1.1
    cat(
      paste("lambda0 < mu0. lambda0 was increased up to ", init_pars[1], "\n")
    )
  }

  if(init_pars[3] < 1){ stop("Initial K must be greater than 1.")}

  N <- length(brts) + 1
  Kprime <- get_Kprime(init_pars)

  if (ceiling(Kprime) < N ) {
    init_pars[3] <-  ceiling(N * (init_pars[1] - init_pars[2]) / init_pars[1])
    cat(paste("K' < N. K was increased up to", init_pars[3], "\n"))
  }
  return(init_pars)

}
