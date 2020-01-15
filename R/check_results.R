#' Check a resultsults data frame does not have duplicated rows
#'
#' Goes through rows and returns a message if two rows are found to be
#' duplicates.
#'
#' @param results a resultsults data frame, as produced by [run_optim()] or accessed by
#' [read_optim_resultsults].
#' @param return_rows logical. Should rows that don't pass the check be
#' returned?
#'
#' @export
#' @author Theo Pannetier
check_duplicated_rows <- function(results, return_rows) {

  duplicated_rows <- which(duplicated(results))
  if (length(duplicated_rows > 0)) {
    print(
      c("The following rows seem to be or have duplicates: ",
        paste(duplicated_rows, collapse = " ")
      )
    )
  }
  if (return_rows) {
    return(duplicated_rows)
  } else {
    return(NULL)
  }

}

#' Check a resultsults data frame for NAs in rows
#'
#' Goes through rows and returns a message if any NA is found in  row.
#'
#' @param results a resultsults data frame, as produced by [run_optim()] or accessed by
#' [read_optim_resultsults].
#' @param return_rows logical. Should rows that don't pass the check be
#' returned?
#'
#' @export
#' @author Theo Pannetier
check_incomplete_rows <- function(results, return_rows){

  incomplete_rows <- c()
  for(i in seq_along(results$mc)){
    if(anyNA(results[i,])){
      incomplete_rows <- c(incomplete_rows, i)
    }
  }
  if(length(incomplete_rows > 0)){
    print(
      c("There are NAs in the following rows: ",
        paste(incomplete_rows, collapse = " ")
      )
    )
  }

  if(return_rows){
    return(incomplete_rows)
  } else {
    return(NULL)
  }

}

#' Check a resultsults data frame for log-likelihood
#'
#' Goes through rows and returns a message if a row lacks a valid
#' log- likelihood value
#'
#' @param results a resultsults data frame, as produced by [run_optim()] or accessed by
#' [read_optim_resultsults].
#' @param return_rows logical. Should rows that don't pass the check be
#' returned?
#'
#' @export
#' @author Theo Pannetier
check_loglikelihood_exists <- function(results, return_rows){

  missing_loglikelihoods <- which(results$loglik %in% c(NA,-1,-Inf))
  if(length(missing_loglikelihoods > 0)){
    print(
      c("There is no log-likelihood estimate for the following mcs:",
        paste(results$mc[missing_loglikelihoods], collapse = " ")
      )
    )
  }

  if(return_rows){
    return(missing_loglikelihoods)
  } else {
    return(NULL)
  }
}

#' Check a resultsults data frame for missing rows
#'
#' Goes through rows and returns a message if any row from 1 to 1000 is missing
#'
#' @param results a resultsults data frame, as produced by [run_optim()] or accessed by
#' [read_optim_resultsults].
#' @param return_rows logical. Should rows that don't pass the check be
#' returned?
#'
#' @export
#' @author Theo Pannetier
check_missing_mcs <- function(results, return_rows){

  missing_mcs <- which(!(1:1000 %in% results$mc))

  if(length(missing_mcs > 0)){
    print(
      c("There is no resultsults for the following mcs: ",
        paste(missing_mcs, collapse = " ")
      )
    )
  }

  if(return_rows){
    return(missing_mcs)
  } else {
    return(NULL)
  }

}



