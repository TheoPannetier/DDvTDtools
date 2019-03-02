#' Check initial parameter values for run_ML
#'
#' Internal function called by \code{run_ML}. The function checks that default
#' or user-specified values entered to initialize the ML optimisation do not break
#' forbidden conditions inside \code{DDD::dd_ML()} and \code{DDD::bd_ML()}.
#'
#' @author Théo Pannetier

check_initpars <- function(init_pars, optim, brts){
  if(!is.numeric(init_pars)){stop("init_pars() should be a numeric vector.")}
  if(optim == "CR" & length(init_pars) != 2){stop("init_pars should have length 2")}
  if(optim %in% c("DD","TD") & length(init_pars) != 3){stop("init_pars should have length 3")}

  if(init_pars[1] < 0){ stop("Initial lambda0 has a negative value.") }
  if(init_pars[2] < 0){ stop("Initial mu0 has a negative value.") }
  if(init_pars[1] <= init_pars[2] ){
    init_pars[1] <- init_pars[2] * 1.1
    cat(paste("lambda0 was step up to ", init_pars[1], "\n"))
  }

  if(optim  %in% c("DD","TD")){

    if(init_pars[3] < 1){ stop("Initial K must be greater than 1.")}

    N <- length(brts) + 1
    Kprime = init_pars[1] / (init_pars[1] - init_pars[2]) * init_pars[3]

    if (ceiling(Kprime) < N ){
      init_pars[3] <-  ceiling(N * (init_pars[1] - init_pars[2]) / init_pars[1])
      cat(paste("K was stepped up to", init_pars[3], "\n"))
      Kprime = init_pars[1] / (init_pars[1] - init_pars[2]) * init_pars[3]
    }
  }
  return(init_pars)

}
