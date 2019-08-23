#' Fit the time-dependent model on Setophaga phylogeny
#'
#' Run the maximum likelihood of the time-dependent model on the phylogeny of
#' Setophaga warblers
#'
#' @param lambda_ini numeric, the initial value for parameter lambda0
#' @param mu_ini numeric, the initial value for parameter mu0
#' @param k_ini numeric, the initial value for parameter K
#' @param outputfile character. Name of the outputfile should be a .rds
#'
#' @author Théo Pannetier
#' @export

fit_td_setophaga <- function(lambda_ini, mu_ini, k_ini, outputfile) {

  if (any(!is.numeric(c(lambda_ini, mu_ini, k_ini)))) {
    stop("Input parameters are not numeric.")
  }
  if (!is.character(outputfile)) {
    stop("outputfile must be a character")
  }

  load(file = "data/setophaga/branching_times.RData")

  out <- DDD::bd_ML(
    brts = brts,
    initparsopt = c(
      lambda_ini,
      mu_ini,
      k_ini
    ),
    idparsopt = 1:3,
    tdmodel = 4,
    optimmethod = "subplex",
    methode = "ode45",
    num_cycles = 5
  )

  fit <- data.frame(
    lambda_ini = lambda_ini,
    mu_ini = mu_ini,
    k_ini = k_ini,
    loglik = out$loglik,
    lambda_ml = out$lambda0,
    mu_ml = out$mu0,
    k_ml = out$lambda1
  )

  saveRDS(fit, file = paste0("data/setophaga/", outputfile))

}
