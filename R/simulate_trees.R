#' Simulate a set of phylogenetic trees under a time-dependent or
#' diversity-dependent model of diversification.
#'
#' Reads a model name and coded parameter input to simulate a dataset of a
#' number of simulated phylogenetic trees.
#'
#' @param sim a character used to select the model, can be either 'DD' or
#' 'TD'.
#' @param para a 4-digit code used to set the parameter values used for the
#' simulation. See \code{parameter_list} for possible values.
#' @param nbmc Integer. How many trees should be generated?
#' @param outputfile path and name for output file, by default generated
#' automatically from \code{sim} and \code{para}.
#' @param save_results logical. Should save the results to \code{outpufile}
#' (default) or not.
#' @param return_results logical. Should results be returned? Default to
#' \code{TRUE}.
#'
#' @return A \code{list} containing itself \code{nbmc} lists corresponding a
#' simulated trees. See \code{DDD:dd_sim} for the content of these lists.
#'
#' @author Cesar Martinez and Theo Pannetier
#'
#' @export

simulate_trees <- function(
  sim,
  para,
  nbmc = 1000,
  outputfile = paste0("./data/sim/sim", sim, "-", para, ".RData"),
  save_results = TRUE,
  return_results = TRUE
  ){
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  if(!is.numeric(nbmc)){
    stop("argument nbmc should be a numeric.")
  }
  if(!is.character(outputfile)){
    stop("argument outputfile should be a character.")
  }
  if(!is.logical(save_results)){
    stop("argument save_results should be a logical")
  }
  if(!is.logical(return_results)){
    stop("argument return_results should be a logical")
  }

  set.seed(42)

  pars <- para_to_pars(para)
  cat("Parameters:", pars, "\n")
  cat(paste("File will be saved at: ", outputfile, "\n"))

  trees <- list()
  for(mc in 1:nbmc)
  {
    cat("Simulating tree ", mc, "...\n")
    if(sim == "DD"){
      trees[[mc]] = DDD::dd_sim(pars[2:4], pars[1])
    } else {
      trees[[mc]] = DDD::td_sim(pars[2:4], pars[1])
    }
    if (save_results){
      save(trees, sim, pars, file = outputfile)
    }
  }
  if (return_results == TRUE){
    return(trees)
  }
}
