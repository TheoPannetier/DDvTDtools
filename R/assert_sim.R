#' Assert the value of 'sim' is a valid model name for DDvTDtools
#'
#' @inheritParams params_doc
#' @author Theo Pannetier
#'
#' @export

assert_sim <- function(sim) {
  if ( !(sim %in% arg_sim())){
    stop(cat("'optim' is invalid. Possible inputs are", arg_sim()))
  }
}
