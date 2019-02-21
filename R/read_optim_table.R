#' Access a table with model fitting results
#'
#' The function will go fetch the data frame corresponding to the arguments and return it.
#'
#' @param sim name of the model used to simulate the trees. See \code{get_sim_names()}
#' @param optim name of the model fitted to the simulated trees. See \code{get_optim_names()}
#' @param init integer, the initialization parameter of model fitting.
#' @param para four-digit-character specifying the parameters used to simulate the trees. See \code{get_para_values}.
#'
#' @author Theo Pannetier
#' @export

read_optim_table <- function(sim, optim, init, para){
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  assert_optim(optim)
  if(!is.numeric(init)){
    stop("init should be a numeric")
  }

  res_filename <- paste0("sim", sim, "_optim", optim, "_init", init, "-", para, ".rds")

  if(!file.exists(paste0("data/optim/", res_filename))){
    stop(paste0("data/optim/",res_filename, " does not exist"))
  }

  res <- readRDS(paste0("data/optim/",res_filename))

  if(!is.data.frame(res)){
    rm(res)
    stop(paste0(paste0("data/optim/",res_filename)," does not contain a data frame."))
  }

  return(res)

}
