#' Fetch values for argument init_k
#'
#' Labels for initial value of K associated with each parameter setting for the
#' main analysis.
#'
#' @author Théo Pannetier
#' @export


fetch_init_k <- function() {
  c(
    "1211" = "true_k",
    "1241" = "true_k",
    "2211" = "true_k",
    "2241" = "true_k",
    "3211" = "true_k",
    "3241" = "true_k",
    "4211" = "from_n",
    "4241" = "from_n",
    "3411" = "from_n",
    "3441" = "true_k"
  )
}
