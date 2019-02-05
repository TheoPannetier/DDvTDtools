#' Predefined pars2 argument for bd_loglik as used in DDvTD
#'
#' Returns default settings to call bd_loglik() for DDvTD, model TD4
#'
#' @author Theo Pannetier
default_pars2_td4 <- function(){
  c(
    4, # tdmodel
    1, # cond
    1, # btorph
    0, # print
    2, # soc
    1000 # max lx
  )
}
