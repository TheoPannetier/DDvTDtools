#' Simulate a set of phylogenetic trees under a time-dependent or diversity-dependent model of diversification.
#'
#' Reads a model name and coded parameter input to simulate a dataset of a number of simulated phylogenetic trees.
#'
#' @param sim_model a character used to select the model, can be either 'DD' or 'TD4.
#' @param para a 4-digit code used to set the parameter values used for the simulation. See \code{parameter_list} for possible values.
#' @param nbmc Integer. How many trees should be generated?
#' @param outputfile path and name for output file, by default generated automatically from \code{sim_model} and \code{para}.
#' @param save_results logical. Should save the results to \code{outpufile} (default) or not.
#' @param return_res logical. Should results be returned? Default to \code{TRUE}.
#'
#' @return A \code{list} containing itself \code{nbmc} lists corresponding a simulated trees. See \code{DDD:dd_sim} for the content of these lists.
#'
#' @author Cesar Martinez and Theo Pannetier
#'
#' @export


simulate_trees <- function(sim_model, para, nbmc = 1000,
                          outputfile = paste0("./data/sim/sim", sim_model,"-",
                                              para,".RData"),
                          save_results = T, return_res = T){

  if(!require('DDD')){install.packages('DDD')}
  set.seed(42)

  pars <- read_para(para)
  cat("Parameters:",pars,"\n")
  cat(paste("File will be saved at: ", outputfile, "\n"))

  trees = list()
  for(mc in 1:nbmc)
  {
    cat("Simulating tree ", mc, "...\n")
    flush.console()
    if(sim_model == "DD"){
      trees[[mc]] = dd_sim(pars[2:4],pars[1])
    } else {
      trees[[mc]] = td_sim(pars[2:4],pars[1])
    }
    if (save_results){
      save(trees,sim_model,pars,file = outputfile)
    }
  }
  if (return_res){
    return(trees)
  }
}
