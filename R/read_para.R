read_para <- function(para){
  ca_vec = c(5,10,15,60)
  la0_vec = c(0.5,0.8,1.4,3.2)
  mu_vec = c(0,0.1,0.2,0.4)
  K_vec = c(40,80,Inf)

  pars = c(ca_vec[as.numeric(substr(para,1,1))],
           la0_vec[as.numeric(substr(para,2,2))],
           mu_vec[as.numeric(substr(para,3,3))],
           K_vec[as.numeric(substr(para,4,4))])
  return(pars)
}
