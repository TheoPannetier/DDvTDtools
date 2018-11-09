check_res_type <- function(res){

  if(!is.factor(res$sim)){ warning("Incorrect data type: res$sim")}
  if(!is.numeric(res$ntips)){ warning("Incorrect data type: res$ntips")}
  if(!is.numeric(res$crown_age)){ warning("Incorrect data type: res$crown_age")}
  if(!is.numeric(res$true_lambda0)){ warning("Incorrect data type: res$true_lambda0")}
  if(!is.numeric(res$true_mu0)){ warning("Incorrect data type: res$true_mu0")}
  if(!is.numeric(res$true_K)){ warning("Incorrect data type: res$true_K")}
  if(!is.integer(res$mc)){ warning("Incorrect data type: res$mc")}
  if(!is.factor(res$optim)){ warning("Incorrect data type: res$optim")}
  if(!is.numeric(res$df)){ warning("Incorrect data type: res$df")}
  if(!is.numeric(res$init)){ warning("Incorrect data type: res$init")}
  if(!is.numeric(res$init_lambda0)){ warning("Incorrect data type: res$init_lambda0")}
  if(!is.numeric(res$init_mu0)){ warning("Incorrect data type: res$init_mu0")}
  if(!is.numeric(res$init_K)){ warning("Incorrect data type: res$init_K")}
  if(!is.numeric(res$conv)){ warning("Incorrect data type: res$conv")}
  if(!is.numeric(res$loglik)){ warning("Incorrect data type: res$loglik")}
  if(!is.numeric(res$AIC)){ warning("Incorrect data type: res$AIC")}
  if(!is.numeric(res$lambda0)){ warning("Incorrect data type: res$lambda0")}
  if(!is.numeric(res$mu0)){ warning("Incorrect data type: res$mu0")}
  if(!is.numeric(res$K)){ warning("Incorrect data type: res$K")}

}
