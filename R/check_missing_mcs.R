check_missing_mcs <- function(res, return_out){

  missing_mcs <- which(!(1:1000 %in% res$mc))

  if(length(missing_mcs > 0)){
    print(
      c("There is no results for the following mcs: ",
        paste(missing_mcs, collapse = " ")
      )
    )
  }

  if(return_out){
    return(missing_mcs)
  } else {
    return(NULL)
  }

}

