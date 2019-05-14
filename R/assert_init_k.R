#' Asserts input init parameter is supported
#'
#' @param init_k a character for the initial parameter setting, typically passed
#' from another function
#'
#' @author Theo Pannetier
#'
#' @export

assert_init_k <- function(init_k) {
  if(!init_k %in% get_possible_init_k()) {
    stop(
      "Input init is incorrect. Call get_possible_init_k() for accepted values"
      )
  }
}
