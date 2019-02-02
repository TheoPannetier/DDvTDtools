#' Assert the value of 'optim' is a valid model name for DDvTDtools
#'
#' @param optim character. The name of a diversification model fitted to data.
#' @author Theo Pannetier
#'
#' @export

assert_optim <- function(optim) {
  if ( !(optim %in% get_optim_names())){
    stop(cat("'optim' is invalid. Possible inputs are", get_optim_names()))
  }
}
