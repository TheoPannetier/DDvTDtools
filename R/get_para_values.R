#' Get possible values of para argument
#'
#' Input values currently accepted for para.
#'    Use \code{para_to_pars} to translate para into paramter values.
#' @param incl_unused logical, specify whether you want to include values that
#' have been considered in earlier phases of the project.
#'
#' @author Theo Pannetier
#' @export
#' @seealso \code{para_to_pars()}

get_para_values <- function(incl_unused = FALSE) {
  if (!is.logical(incl_unused)) {
    stop("incl_unused must be logical")
    }
  if (!incl_unused) {
    c(
      1211, 1241,
      2211, 2241,
      3211, 3241,
      4211, 4241
    )
  } else {
    c(
      1211, 1221, 1231, 1241,
      2211, 2221, 2231, 2241,
      3211, 3221, 3231, 3241,
      4211, 4221, 4231, 4241,
      3411, 3421, 3431, 3441
    )
  }
}
