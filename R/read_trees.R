#' Load a simulated trees dataset
#'
#' Loads a file containing simulated trees at the specified location. Target file must follow nomeclature 'simXX-para.RData
#'
#' @param dir character, path to the directory where the target file is located.
#' @param sim name of the model used to simulate the trees. See \code{get_sim_names()}
#' @param para four-digit-character specifying the parameters used to simulate the trees. See \code{get_para_values}.
#'
#' @author Theo Pannetier
#' @export

read_trees <- function(dir, sim, para){
  if(!is.character(dir)){stop('dir must be a character.')}
  assert_para(para)
  assert_sim(sim)

  filename = paste0("sim", sim, "-", para, ".RData")

  if(!file.exists(paste0(dir,filename))){
    stop(paste0(dir,filename, " does not exist"))
  }

  load(paste0(dir, filename))

  # Assert we are dealing with the right class of object
  if(!is.binary.phylo(trees[[1]][[1]])){
    stop(paste0(dir,filename, " is not a list of binary phylo objects."))
  }
  return(trees)
}
