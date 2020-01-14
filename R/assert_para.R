#' Assert the value of 'para' is a valid parameter combination for DDvTDtools
#'
#' @inheritParams params_doc
#' @author Theo Pannetier
#' @seealso \code{get_para_values()}, \code{para_to_pars()}
#'
#' @export

assert_para <- function(para) {
  if ( !(para %in% DDvTDtools::get_para_values())){
    stop("'para' is invalid. Try get_para_values() to see accepted model names")
  }
}
