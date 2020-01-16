#' Assert the value of 'optim' is a valid model name for DDvTDtools
#'
#' @inheritParams params_doc
#' @author Theo Pannetier
#'
#' @export

assert_optim <- function(optim) {
  if ( !(optim %in% arg_optim())){
    stop(cat("'optim' is invalid. Possible inputs are", arg_optim()))
  }
}
