#' Get values for argument init_k
#'
#' Labels for initial value of K associated with each parameter setting for the
#' main analysis.
#' @param incl_unused logical, specify whether you want to include values that
#' have been considered in earlier phases of the project.

#' @author Théo Pannetier
#' @export


get_init_k <- function(incl_unused = FALSE) {
  if (!is.logical(incl_unused)) {
    stop("incl_unused must be logical")
  }
  if (!incl_unused) {
    c(
      "1211" = "true_k",
      "1241" = "true_k",
      "2211" = "true_k",
      "2241" = "true_k",
      "3211" = "from_n",
      "3241" = "true_k",
      "4211" = "from_n",
      "4241" = "from_n"
    )
  } else {
    c(
      "1211" = "true_k",
      "1241" = "true_k",
      "2211" = "true_k",
      "2241" = "true_k",
      "3211" = "from_n",
      "3241" = "true_k",
      "4211" = "from_n",
      "4241" = "from_n",
      "3411" = "from_n",
      "3441" = "true_k"
    )
  }

}
