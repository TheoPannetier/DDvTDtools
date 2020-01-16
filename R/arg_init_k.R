#' Accepted values of argument init_k
#'
#' Returns posssible value of init_k (initialisation value of K for model
#' optimisation). For \code{true_k} K is initialised at the true value (40),
#' for \code{from_n} it is initialised at N, the nb  of tips in the tree
#'
#' @author Theo Pannetier
#'
#' @export
arg_init_k <- function() {
  c("true_k", "from_n")
}
