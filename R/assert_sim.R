#' Assert the value of 'sim' is a valid model name for DDvTDtools
#'
#' @param sim character. The name of a simulation model
#' @author Theo Pannetier

assert_sim <- function(sim) {
  if ( !(sim %in% DDvTDtools::get_sim_names())){
    stop("'sim' is invalid. Try get_sim_names() to see accepted model names")
  }
}
