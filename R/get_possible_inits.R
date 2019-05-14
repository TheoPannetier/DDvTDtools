#' Accepted values of argument init
#'
#' Returns posssible value of init (initialisation setting). For \code{true_k}
#' K is initialised at the true value (40), for \code{from_n} it is initialised
#' at N, the nb  of tips in the tree
#'
#' @author Theo Pannetier
#'
#' @export
get_possible_inits <- function() {
  c("true_k", "from_n")
}
