check_metadata <- function(res, sim, para, optim, init){

  pars <- DDvTDtools:::read_para(para)

  if(any(res$sim != sim)){
    print("res$sim does not match optim argument !")
  }

  if(any(res$optim != optim)){
    print("res$optim does not match sim argument !") }

  if(any(res$init != init)){ print("res$init does not match init argument !")
  }

  if(any(res$crown_age != pars[1])){
    print("res$crown_age does not match para argument !")
  }

  if(any(res$true_lambda0 != pars[2])){
    print("res$true_lambda0 does not match para argument !") }
  if(any(res$true_mu0 != pars[3])){ print("res$true_mu0 does not match para argument !")
  }

  if(any(res$true_K != pars[4])){
    print("res$true_K does not match para argument !")
  }

}
