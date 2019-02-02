#' Assert the value of 'sim' is a valid model name for DDvTDtools
#'
#' @param sim character. The name of a simulation model
#' @author Theo Pannetier
#'
#' @export

assert_sim <- function(sim) {
  if ( !(sim %in% get_sim_names())){
    stop(cat("'optim' is invalid. Possible inputs are", get_sim_names()))
  }
}
