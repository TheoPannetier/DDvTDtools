check_duplicated_rows <- function(res, return_out){

  duplicated_rows <- which(duplicated(res))
  if(length(duplicated_rows > 0)){
    print(
      c("The following rows seem to be or have duplicates: ",
        paste(duplicated_rows, collapse = " ")
      )
    )
  }

  if(return_out){
    return(duplicated_rows)
  } else {
    return(NULL)
  }

}
