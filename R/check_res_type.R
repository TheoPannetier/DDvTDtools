check_res_type <- function(res){

  if(!is.factor(res$sim)){ print("Incorrect data type: res$sim")}
  if(!is.numeric(res$ntips)){ print("Incorrect data type: res$ntips")}
  if(!is.numeric(res$crown_age)){ print("Incorrect data type: res$crown_age")}
  if(!is.numeric(res$true_lambda0)){ print("Incorrect data type: res$true_lambda0")}
  if(!is.numeric(res$true_mu0)){ print("Incorrect data type: res$true_mu0")}
  if(!is.numeric(res$true_K)){ print("Incorrect data type: res$true_K")}
  if(!is.integer(res$mc)){ print("Incorrect data type: res$mc")}
  if(!is.factor(res$optim)){ print("Incorrect data type: res$optim")}
  if(!is.numeric(res$df)){ print("Incorrect data type: res$df")}
  if(!is.numeric(res$init)){ print("Incorrect data type: res$init")}
  if(!is.numeric(res$init_lambda0)){ print("Incorrect data type: res$init_lambda0")}
  if(!is.numeric(res$init_mu0)){ print("Incorrect data type: res$init_mu0")}
  if(!is.numeric(res$init_K)){ print("Incorrect data type: res$init_K")}
  if(!is.numeric(res$conv)){ print("Incorrect data type: res$conv")}
  if(!is.numeric(res$loglik)){ print("Incorrect data type: res$loglik")}
  if(!is.numeric(res$AIC)){ print("Incorrect data type: res$AIC")}
  if(!is.numeric(res$lambda0)){ print("Incorrect data type: res$lambda0")}
  if(!is.numeric(res$mu0)){ print("Incorrect data type: res$mu0")}
  if(!is.numeric(res$K)){ print("Incorrect data type: res$K")}

}
