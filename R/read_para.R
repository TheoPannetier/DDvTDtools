read_para <- function(para){

  # Possible values, to be transferred to /data
  data("parameter_list")
  pars = c(parameter_list$crown_age[as.numeric(substr(para,1,1))],
           parameter_list$lambda[as.numeric(substr(para,2,2))],
           parameter_list$mu[as.numeric(substr(para,3,3))],
           parameter_list$K[as.numeric(substr(para,4,4))]
  )
  return(pars)
}
