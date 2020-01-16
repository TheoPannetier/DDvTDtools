#' Default DD pars2 argument passed to DDD
#'
#' Default values for the `pars2` argument of [DDD::dd_loglik]
#'
#' @author Theo Pannetier
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
