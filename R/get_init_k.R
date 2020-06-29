#' Get values for argument init_k
#'
#' Labels for initial value of K associated with each parameter setting for the
#' main analysis.
#' @author Théo Pannetier
#' @export


get_init_k <- function() {
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
    "3441" = "true_k",
    "3212" = "from_n",
    "3242" = "from_n",
    "1221" = "from_n",
    "1231" = "from_n",
    "2221" = "from_n",
    "2231" = "from_n",
    "3221" = "from_n",
    "3231" = "from_n",
    "4221" = "from_n",
    "4231" = "from_n",
    "3222" = "from_n",
    "3232" = "from_n"
  )

}
