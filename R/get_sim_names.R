#' Get possible values of sim argument
#'
#' Names of the simulation models currently accepted as arguments by various functions of \code{DDvTDtools}. Based on \code{becosys::get_bd_param_names} from Richel J.C. Bilderbeek.
#'
#' @author Theo Pannetier
#' @export

get_sim_names <- function() {
  c("DD", "TD")
}
