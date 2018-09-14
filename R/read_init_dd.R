read_init_dd <- function(pars, init){
  check_init(init)
  
  if(init == 1){
    pars = pars
  } else if (init == 2){
    pars = c(pars[1:3], 1000 )
  } else if (init == 3){
    pars = c(pars[1:3], length(brts) + 1)
  }
  pars <- pmin(pars,1000)
  return(pars)
}