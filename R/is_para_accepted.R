#' Is the value of 'para' a valid parameter combination for DDvTDtools?
#'
#' @param para numeric or character. A four-digits code specifying a set of parameter values.
#' @author Theo Pannetier
#' @seealso \code{get_para_values()}, \code{para_to_pars()}

is_para_accepted <- function(para) {
  if ( !(para %in% DDvTDtools::get_para_values())){
    stop("'para' is invalid. Try get_para_values() to see accepted model names")
  }
}
