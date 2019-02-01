#' Is the value of 'sim' a valid model name for DDvTDtools?
#'
#' @param sim character. The name of a simulation model
#' @author Theo Pannetier

is_sim_accepted <- function(sim) {
  if ( !(sim %in% DDvTDtools::get_sim_names())){
    stop("'sim' is invalid. Try get_sim_names() to see accepted model names")
  }
}
