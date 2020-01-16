#' Default TD pars2 argument passed to DDD
#'
#' Default values for the `pars2` argument of [DDD::bd_loglik]
#'
#' @author Theo Pannetier
#' @export
default_pars2_td4 <- function(){
  c(
    4, # tdmodel
    1, # cond
    1, # btorph
    0, # verbose
    2, # soc
    1000 # max lx
  )
}
