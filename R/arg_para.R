#' Get possible values of argument para
#'
#' Input values currently accepted for argument `para`.
#'
#' Use [para_to_pars()] to translate `para` into parameter values.
#'
#' @param incl_unused logical, specify whether you want to include values that
#' have been considered in earlier phases of the project.
#'
#' @details Each value is a four-digit number coding for a set of parameter
#' values. In order: crown age, baseline speciation rate
#' (\eqn{\lambda_0}{\lambda_0}), extinction rate (\eqn{\mu_0}{\mu_0}),
#' and carrying capacity (K). Values are encoded as follows:
#' \tabular{ccccc}{
#'    \strong{digit} \tab \strong{crown_age} \tab \strong{\eqn{\lambda_0}}
#'    \tab \strong{\eqn{\mu_0}} \tab \strong{K} \cr
#'    1 \tab 5  \tab  -   \tab 0   \tab 40 \cr
#'    2 \tab 10  \tab 0.8 \tab -   \tab 80 \cr
#'    3 \tab 15  \tab -   \tab -   \tab - \cr
#'    4 \tab 60  \tab -   \tab 0.4 \tab -
#' }
#'
#' For example, `para = 3241` denotes the following parameter set:
#' * crown age = 15 myr
#' * \eqn{\lambda_0} = 0.8
#' * \eqn{\mu_0} = 0.4
#' * K = 40.
#'
#' This `para` code can be converted to parameter values with [para_to_pars()].
#'
#' Note that many cells in the table above are left empty, and only three
#' parameters actually change value across parameter sets. These empty cells
#' correspond to parameter values that were considered in earlier steps of the
#' project, and later abandonned.
#'
#' @author Theo Pannetier
#' @export
#' @seealso \code{para_to_pars()}

arg_para <- function(incl_unused = FALSE) {
  if (!is.logical(incl_unused)) {
    stop("incl_unused must be logical")
    }
  if (!incl_unused) {
    c(
      1211, 1241,
      2211, 2241,
      3211, 3241,
      4211, 4241,
      3212, 3242,
      3411, 3441
    )
  } else {
    c(
      1211, 1221, 1231, 1241,
      2211, 2221, 2231, 2241,
      3211, 3221, 3231, 3241,
      4211, 4221, 4231, 4241,
      3411, 3421, 3431, 3441,
      3212, 3242
    )
  }
}
