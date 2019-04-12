#' Extract a set of branching times from a dataset of simulated trees
#'
#' Read the dataset at specified location and returns branching times for specified tree.
#'
#' @param sim character, the name of a simulation model. See 
#' \code{get_sim_names()} for possible values.
#' @param para numeric or character. A four-digits code specifying a set of 
#' parameter values. See \code{get_para_values()}
#' @param mc numeric. The index of the tree to return branching times from.
#' @param with_extinct logical. \code{with_extinct = F} for the reconstructed 
#' tree, \code{with_extinct = T} for the complete tree.
#'
#' @return a numerical vector containing the branching times of the tree.
#'
#' @author Theo Pannetier
#' @export

get_brts <- function(sim, para, mc, with_extinct = F){
  assert_DDvTD_wd()
  assert_para(para)
  assert_sim(sim)
  if(!is.numeric(mc) | !mc %in% 1:1000 ){
    stop('mc must be a numeric in 1:1000')
  }
  if(!is.logical(with_extinct)){
    stop('with_extinct must be a logical')
  }

  trees <- read_trees(sim = sim, para = para)

  if(any(!mc %in% seq_along(trees))){
    stop('There is no tree is the file for that mc.')
  }

  if(!with_extinct){
    # return brts for reconstructed tree
    brts <- as.numeric( ape::branching.times( trees[[mc]][[1]] ) )
  } else {
    # return brts for complete tree
    brts <- as.numeric( ape::branching.times( trees[[mc]][[2]] ) )
  }

  rm(trees)

  return(brts)
}
