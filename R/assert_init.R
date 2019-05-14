#' Asserts input init parameter is supported
#'
#' @param init a character for the initial parameter setting, typically passed
#' from another function
#'
#' @author Theo Pannetier
#'
#' @export

assert_init <- function(init) {
  if(!init %in% get_possible_inits()) {
    stop(
      "Input init is incorrect. Call get_possible_inits() for accepted values"
      )
  }
}
