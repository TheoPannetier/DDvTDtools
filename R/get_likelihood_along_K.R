#' Compute the likelihood landscape along the K-axis
#'
#' Returns the sequence of (log-)likelihood estimates of a tree under DD or TD
#' for a vector of values of parameter K.
#'
#' @param optim character. The name of the model to fit to the simulated
#' phylogenies, either "DD" or "TD".
#' @param brts numeric vector, a set of branching times.
#' @param lambda0 numeric. A (fixed) value for the speciation rate.
#' @param mu0 numeric. A (fixed) value for the extinction rate.
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
get_likelihood_along_K <- function(optim,
                            brts,
                            lambda0,
                            mu0,
                            K_seq = seq(10, 1000, by = 10)) {
  
  # Check input ----------------------------------------------------------------
  assert_optim(optim)
  if (optim == "CR") {
    stop("function not intended for optim = CR. Please choose either DD or TD.")
  }
  if (!is.numeric(brts) | length(brts) < 2){
    stop("brts must be a numeric vector.")
  }
  if (!is.numeric(lambda0) | lambda0 < 0){
    stop("lambda0 must be a positive numeric value.")
  }
  if (!is.numeric(mu0) | mu0 < 0){
    stop("mu0 must be a positive numeric value.")
  }
  
  # Global variables -----------------------------------------------------------
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
  cbind("lambda0" = lambda0, "mu0" = mu0, "K" = K_seq, "loglik" = loglik_seq)
}
