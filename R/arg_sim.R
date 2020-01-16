#' Possible values of argument sim
#'
#' Names of the simulation models currently accepted as arguments by various
#' functions of \code{DDvTDtools}.
#'
#' @details DD refers to the diversity-dependent model, TD to the time-dependent
#' model.
#'
#' @author Theo Pannetier
#' @export

arg_sim <- function() {
  c("DD", "TD")
}
