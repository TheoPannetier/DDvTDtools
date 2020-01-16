#' Assert the value of 'para' is a valid parameter combination for DDvTDtools
#'
#' @inheritParams params_doc
#' @author Theo Pannetier
#' @seealso \code{arg_para()}, \code{para_to_pars()}
#'
#' @export

assert_para <- function(para) {
  if ( !(para %in% DDvTDtools::arg_para())){
    stop("'para' is invalid. Try arg_para() to see accepted model names")
  }
}
