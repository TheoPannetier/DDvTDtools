#' Extract multiple sets of branching times from a dataset of simulated trees
#'
#' Read the dataset at specified location and returns branching times for multiple trees specified by indices.
#'
#' @param sim character, the name of a simulation model. See \code{get_sim_names()} for possible values.
#' @param para numeric or character. A four-digits code specifying a set of parameter values. See \code{get_para_values()}
#' @param rangemc numeric vector. The indices of the trees to return branching times from.
#' @param with_extinct logical. \code{with_extinct = F} for the reconstructed tree, \code{with_extinct = T} for the complete tree.
#'
#' @return a list of numerical vectors containing the branching times of the specified trees.
#'
#' @author Theo Pannetier
#' @export

get_multi_brts <- function( sim, para, rangemc, with_extinct = F) {
  assert_DDvTD_wd()
  assert_para(para)
  assert_sim(sim)
  if(!is.numeric(rangemc) | any(!rangemc %in% 1:1000 )){
    stop('mc must be a vector of numeric values between 1 and 1000')
  }
  if(!is.logical(with_extinct)){
    stop('with_extinct must be a logical')
  }

  brts_bundle = list()

  trees <- read_trees(sim = sim, para = para)

  if(any(!rangemc %in% seq_along(trees))){
    stop('rangemc out of bounds of the trees list.')
  }

  for (i in seq_along(rangemc)){

    mc = rangemc[i]

    if(!with_extinct){
      # return brts for reconstructed tree
      brts_bundle[[i]] <- as.numeric( ape::branching.times( trees[[mc]][[1]] ) )
    } else{
      # return brts for complete tree
      brts_bundle[[i]] <- as.numeric( ape::branching.times( trees[[mc]][[2]] ) )
    }
  }

  rm(trees)

  return(brts_bundle)
}
