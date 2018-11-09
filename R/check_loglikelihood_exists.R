check_loglikelihood_exists <- function(res, return_out){

  missing_loglikelihoods <- which(res$loglik %in% c(NA,-1,-Inf))
  if(length(missing_loglikelihoods > 0)){
    warning(
      c("There is no log-likelihood estimate for the following mcs:",
        paste(res$mc[missing_loglikelihoods], collapse = " ")
      )
    )
  }

  if(return_out){
    return(missing_loglikelihoods)
  } else {
    return(NULL)
  }
}
