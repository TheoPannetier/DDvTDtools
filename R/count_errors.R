#' Count errors in a log file
#' 
#' Counts the number of lines in a text file matching specified errors.
#' 
#' @param log_file a log file to search the errors in. \code{.txt} or 
#' \code{.log} files work, maybe other formats?
#' @param error_templates a character vector with the error patterns to look 
#' for.
#' The match must be exact for an error to be counted.
#' 
#' @return a vector of the same length as \code{error_templates} giving 
#' the number of counts of each error.
#' 
#' @author Théo Pannetier
#' @export
count_errors <- function(log_filename, error_templates){
  log_file <- readLines(log_filename)
  counts <- rep(0, length(error_templates))
  
  for(i in seq_along(log_file)){
    for(j in seq_along(error_templates)){
      if(log_file[[i]] == error_templates[j]){
        counts[j] <- counts[j] +1
      }
    }
  }
  return(counts)
}


