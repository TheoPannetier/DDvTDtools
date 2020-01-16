#' Accepted values of argument init_k
#'
#' Returns posssible value of init_k (initialisation value of K for model
#' optimisation).
#'
#' @details `true_k` refers to optimisation results initialised with K set to
#' the true value (40), i.e. results produced with [run_optim()]
#' `from_n` refers to optimisation results initialised with K set to
#'  the nb of tips in a tree, i.e. results produced with [run_optim_from_n()]
#'
#' @author Theo Pannetier
#' @export

arg_init_k <- function() {
  c("true_k", "from_n")
}
