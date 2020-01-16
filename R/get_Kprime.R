#' Compute K' from la0, mu0, K
#'
#' @param pars a numeric vector containing `c(la0, mu0, K)`
#'
#' @author Théo Pannetier
#' @export
#'
get_Kprime <- function(pars){
  if(!is.numeric(pars)|length(pars) != 3){stop("Invalid input. Please check pars argument")}
  pars[1] * pars[3] / (pars[1] - pars[2])
  # Kprime = la0 * K / (la0 - mu0)
}
