#' Compute the likelihood landscape along the K-axis
#'
#' Returns the sequence of (log-)likelihood estimates of a tree under DD or TD
#' for a vector of values of parameter K.
#'
#' @param sim character, the name of a simulation model. See
#' \code{get_sim_names()} for possible values.
#' @param optim character. The name of the model to fit to the simulated
#' phylogenies. See \code{get_optim_names()} for possible inputs.
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values. See \code{get_para_values()}
#' @param mc numeric. The index of the tree to return branching times from.
#' @param K_seq numeric vector, the values of parameter K to return values for.
#'
#' @details Values of the two other parameters of the model (\code{lamba0} and
#' \code{mu0}) are taken from the maximum likelihood estimate. These values are
#' fetched from the results data frame with \code{read_optim_table}, so you need
#' to make sure they exist and that the file matches the package syntax to run
#' the function.
#'
#' @author Théo Pannetier
#' @export
get_likelihood_along_K <- function(sim, optim, para, mc,
                                   lambda0, mu0,
                                   K_seq = seq(10, 1000, by = 10)) {

  # Check input ----------------------------------------------------------------
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  assert_optim(optim)

  if (optim == "CR") {
    stop("function not intended for optim = CR. Please choose either DD or TD.")
  }
  if (!(mc %in% 1:1000)) {
    stop("argument mc must be a numeric between 1 and 1000.")
  }
  if (!is.numeric(lambda0) | lambda0 < 0){
    stop("lambda0 must be a positive numeric value.")
  }
  if (!is.numeric(mu0) | mu0 < 0){
    stop("mu0 must be a positive numeric value.")
  }

  # Global variables -----------------------------------------------------------
  brts <- get_brts(sim = sim, para = para, mc = mc)
  missnumspec <- 0
  methode <- "ode45"

  # Compute loglik along K_seq -------------------------------------------------
  loglik_seq <- c()
  for (K in K_seq) {

    pars1 <- c(lambda0, mu0, K)

    if (optim == "DD") {
      loglik <- DDD::dd_loglik(
        pars1 = pars1,
        pars2 = default_pars2_dd1(),
        brts = brts,
        missnumspec = missnumspec,
        methode = methode
      )
    } else if (optim == "TD") {
      loglik <- DDD::bd_loglik(
        pars1 = pars1,
        pars2 = default_pars2_td4(),
        brts = brts,
        missnumspec = missnumspec,
        methode = methode
      )
    }
    loglik_seq <- c(loglik_seq, loglik)
  }

  # Return results -------------------------------------------------------------
  cbind("K" = K_seq, "loglik" = loglik_seq, "likelihood" = exp(loglik_seq))
}
