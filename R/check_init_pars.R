#' Check initial parameter values for run_ML
#'
#' Checks that default or user-specified values entered to initialize the
#' ML optimisation do not break forbidden conditions inside \code{DDD::dd_ML()}
#' and \code{DDD::bd_ML()}.
#'
#' @param init_pars numeric vector. The initial parameter values to be checked.
#' @param optim character, the model to fit on the tree.
#' Call \code{get_optim_names()} for possible values.
#' @param brts numeric vector. The branching times for the tree to fit model
#' \code{optim} on.
#'
#'
#' @author Théo Pannetier
#'
#' @export

check_init_pars <- function(init_pars, optim, brts){
  assert_optim(optim)
  if(!is.numeric(init_pars)){stop("brts should be a numeric vector.")}
  if(!is.numeric(brts)){stop("init_pars should be a numeric vector.")}
  if(optim == "CR" & length(init_pars) != 2){
    stop("init_pars should have length 2")
    }
  if(optim %in% c("DD","TD") & length(init_pars) != 3){
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

  if(optim %in% c("DD","TD")){

    if(init_pars[3] < 1){ stop("Initial K must be greater than 1.")}

    N <- length(brts) + 1
    Kprime = init_pars[1] / (init_pars[1] - init_pars[2]) * init_pars[3]

    if (ceiling(Kprime) < N ) {
      init_pars[3] <-  ceiling(N * (init_pars[1] - init_pars[2]) / init_pars[1])
      cat(paste("K' < N. K was increased up to", init_pars[3], "\n"))
      Kprime = init_pars[1] / (init_pars[1] - init_pars[2]) * init_pars[3]
    }
  }
  return(init_pars)

}
