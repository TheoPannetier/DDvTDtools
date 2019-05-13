#' Cure format for erroneous results
#'
#' Changes the format of results for trees on which optimisation failed, e.g.
#' changing loglik = -1 to loglik = -Inf.
#'
#' @param df a results data frame, i.e. as read by
#'  \code{DDvTDtools::read_optim_table()}
#'
#' @author Theo Pannetier
#' @export

cure_error_rows <- function(df) {
  if (!is.data.frame(df)) {
    stop("Incorrect input type. df must be a data frame.")
  }

  outdated_error_rows <- df$mc[which(df$loglik == -1)]
  if (length(outdated_error_rows) > 0) {
    for(i in outdated_error_rows) {
      df$loglik[i] <- -Inf
      df$AIC[i] <- Inf
      df$lambda0_ML[i] <- NA
      df$mu0_ML[i] <- NA
      df$K_ML[i] <- NA
    }
  }
  df

}
