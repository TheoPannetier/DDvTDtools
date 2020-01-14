#' Translate 'para' argument into parameter values
#'
#' Prints the parameter values associated with a given value of argument 'para'.
#'
#' @inheritParams params_doc
#'
#' @return a four-element numeric vector containing the parameter values associated to 'para'.
#'
#' @author Theo Pannetier
#'
#' @export
#'
para_to_pars <- function(para){

  assert_para(para)

  crown_age = c(5, 10, 15, 60)
  lambda0 = c(0.4, 0.8, 1.4, 3.2)
  mu0 = c(0, 0.1, 0.2, 0.4)
  K = c(40, 80)

  pars = c(
    "crown_age" = crown_age[as.numeric(substr(para,1,1))],
    "lambda0" = lambda0[as.numeric(substr(para,2,2))],
    "mu0" = mu0[as.numeric(substr(para,3,3))],
    "K" = K[as.numeric(substr(para,4,4))]
  )

  return(pars)
}
