#' Fix indexing of results data frame
#'
#' Checks if rows in the results data frame are indexed correctly
#' (from 1 to 1000), and, if not, re-orders them accordingly.
#'
#' @param results a results data frame, as produced by [run_optim()] or accessed
#' through [read_optim_results()].
##'
#' @author Theo Pannetier
#' @export

fix_indexing <- function(results) {
  if (!is.data.frame(results)) {
    stop("Incorrect input type. results must be a data frame.")
  }

  missing_rows <- which(!(1:1000 %in% results$mc))
  if (length(missing_rows) > 0) {
    stop(
      "Input data frame has missing rows.
 Fill them with DDvTDtools::fill_missing_rows before continuing."
    )
  }
  if (any(!results$mc == seq(1, 1000))) {
    cat("Row order incorrect, fixing indexing...\n")
    results_copy <- results
    for (i in 1:1000) {
      results_copy[i,] <- results[which(results$mc == i),]
    }
    if (any(!results_copy$mc == seq(1, 1000))) {
      stop("Failed to fix indexing.")
    }
    results <- results_copy
  } else {
    cat("Row order is correct, nothing to fix here\n")
  }
  results
}
