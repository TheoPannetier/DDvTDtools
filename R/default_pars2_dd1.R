#' Predefined pars2 argument for dd_loglik as used in DDvTD
#'
#' Returns default settings to call dd_loglik() for DDvTD, model DD1
#'
#' @author Theo Pannetier
#'
#' @export

default_pars2_dd1 <- function(){
  c(
    1000, # max lx
    1, # ddmodel
    1, # cond
    1, # btorph
    0, # verbose
    2  # soc
  )
}
