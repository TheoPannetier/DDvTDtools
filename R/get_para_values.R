#' Get possible values of para argument
#'
#' Input values currently accepted for para.
#'    Use \code{para_to_pars} to translate para into paramter values.
#'
#' @author Theo Pannetier
#' @export
#' @seealso \code{para_to_pars()}

get_para_values <- function(){
  c(
    1211, 1221, 1231, 1241,
    2211, 2221, 2231, 2241,
    3211, 3221, 3231, 3241,
    4211, 4221, 4231, 4241,
    3411, 3421, 3431, 3441
    )
}
