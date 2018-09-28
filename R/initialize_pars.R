initialize_pars <- function(pars, init, init_pars, sim_model, optim_model, mc, brts){
  # accepted init
  data("init_tags")
  # translate para into parameter values
  # pars <- read_para(para) # already loaded with the trees

  if ( !( init %in% init_tags) ){
    stop(paste("init value incorrect. Call data(init_tags) for accepted values"))
  }

  if(init == 0){
    if(optim_model %in% c("DD", "TD") & length(init_pars) != 3){stop("incorrect input in init_pars")}
    if(optim_model == "CR" & length(init_pars) != 2){stop("incorrect input in init_pars")}
    init_pars = init_pars

  } else if(init == 1){
    init_pars = pars[2:4]

  } else if (init == 2){

    res_optimCR <- readRDS(paste0("./data/optim/sim",sim_model,"_optimCR_init1-",para,".rds"))
    int_pars = c(
      res_optimCR$lambda0[mc],
      res_optimCR$mu0[mc],
      1000 # K
    )
  } else if (init == 3){

    res_optimCR <- readRDS(paste0("./data/optim/sim",sim_model,"_optimCR_init1-",para,".rds"))
    init_pars = c(
      res_optimCR$lambda0[mc],
      res_optimCR$mu0[mc],
      length(brts) + 1 # K
    )

  } else if (init == 4){

    init_pars = c(pars[2:3], length(brts) + 1)

  } else if (init == 5){

    init_pars = c(pars[2]*0.5, pars[3])

  }

  init_pars <- pmin(pars,1000)

  return(init_pars)

}
