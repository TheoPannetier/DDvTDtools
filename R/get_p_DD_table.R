#' Get p_DD values for each model and parameter setting
#'
#' Returns values of p_DD (see \code{get_p_DD}) for both models and all parameter
#' settings used in the main analysis
#'
#' @author Théo Pannetier
#' @export

get_p_DD_table <- function() {

  paras <- c(1211, 1241, 2211, 2241, 3211, 3241, 4211, 4241, 3411, 3441)
  init_ks <- fetch_init_k()

  p_DD_table <- dplyr::tibble(
    "DD" = mapply(
      FUN = function(para, init_k) {
        get_p_DD(sim = "DD", para = para, init_k = init_k)
      },
      paras,
      init_ks
    ),
    "TD" = mapply(
      FUN = function(para, init_k) {
        get_p_DD(sim = "TD", para = para, init_k = init_k)
      },
      paras,
      init_ks
    )
  )
  row.names(p_DD_table) <- paras
  return(p_DD_table)
}

