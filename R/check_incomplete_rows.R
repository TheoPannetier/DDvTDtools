check_incomplete_rows <- function(res, return_out){

  incomplete_rows <- c()
  for(i in seq_along(res$mc)){
    if(anyNA(res[i,])){
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

  if(return_out){
    return(incomplete_rows)
  } else {
    return(NULL)
  }

}
