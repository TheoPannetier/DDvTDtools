#' Fix indexing of results data frame
#'
#' Checks if the row are indexed correctly (from 1 to 1000), and, if not, re-
#' orders them accordingly.
#'
#' @param df a results data frame, i.e. as read by
#'  \code{DDvTDtools::read_optim_table()}
#'
#' @author Theo Pannetier
#' @export

fix_indexing <- function(df) {
  if (!is.data.frame(df)) {
    stop("Incorrect input type. df must be a data frame.")
  }

  missing_rows <- which(!(1:1000 %in% df$mc))
  if (length(missing_rows) > 0) {
    stop(
      "Input data frame has missing rows.
 Fill them with DDvTDtools::fill_missing_rows before continuing."
    )
  }
  if (any(!df$mc == seq(1, 1000))) {
    cat("Row order incorrect, fixing indexing...\n")
    df_copy <- df
    for (i in 1:1000) {
      df_copy[i,] <- df[which(df$mc == i),]
    }
    if (any(!df_copy$mc == seq(1, 1000))) {
      stop("Failed to fix indexing. Check your code, Theo!")
    }
    df <- df_copy
  } else {
    cat("Row order is correct, nothing to fix here\n")
  }
  df
}
