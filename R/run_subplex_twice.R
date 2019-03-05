#' Test running optimisation twice
#'
#' A test function that calls subplex to optimise the ML, then runs it a second time
#' starting from the parameter estimates from the previous run.
#'
#' @param sim character, the simulation model. Call \code{get_sim_names()} for a list of possible values.
#' @param para character or numeric, the code specifying the parameter values used in the simulation.
#' Call \code{get_para_values()} for possible inputs, and \code{para_to_pars()} for the corresponding parameter values.
#' @param optim character. The name of the model to fit to the simulated phylogenies. See \code{get_optim_names()} for possible inputs.
#'
#' @return a 3-element numeric vector containing the loglik after the first run, second run, and imporvement
#' between the two (second - first)
#' @author Théo Pannetier
#'
#' @export

run_subplex_twice <- function(sim, para, optim, mc, verbose = 0, methode = "ode45"){
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  assert_optim(optim)

  brts <- get_brts(sim = sim, para = para, mc = mc)

  init_pars <- para_to_pars(para)[2:4]
  init_pars <- check_initpars(init_pars, optim, brts)

  logliks <- c()

  for(i in 1:2){
    if(optim == "DD"){
      ML <- DDD::dd_ML(
        brts = brts,
        initparsopt = init_pars,
        idparsopt = seq_along(init_pars),
        methode = methode,
        verbose = verbose
      )
    } else if (optim == "TD"){
      ML <- DDD::bd_ML(
        brts = brts,
        initparsopt = init_pars,
        idparsopt = seq_along(init_pars),
        tdmodel = 4,
        methode = methode,
        verbose = verbose
      )
    }
    logliks <- c(logliks, ML$loglik)
    init_pars <-c(ML[1,1], ML[1,2], ML[1,3])
  }
  logliks <- c(logliks, logliks[2]-logliks[1])
  if (any(logliks == -1)){logliks[3] <- NA}
  cat(paste("Loglik 1st run = ", logliks[1]),"\n")
  cat(paste("Loglik 2nd run = ", logliks[2]),"\n")
  cat(paste("Improvement = ", logliks[3]),"\n")
  return(logliks)
}

