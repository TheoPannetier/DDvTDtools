#' Asserts input init parameter is supported
#'
#' @inheritParams params_doc
#'
#' @author Theo Pannetier
#'
#' @export

assert_init_k <- function(init_k) {
  if(!init_k %in% arg_init_k()) {
    stop(
      "Input init is incorrect. Call arg_init_k() for accepted values"
      )
  }
}
