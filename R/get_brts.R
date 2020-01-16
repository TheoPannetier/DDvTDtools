#' Extract a set of branching times from a dataset of simulated trees
#'
#' Read the dataset and returns the branching times for the specified tree.
#'
#' @inheritParams params_doc
#' @return a numerical vector containing the branching times of the tree.
#'
#' @author Theo Pannetier
#' @export

get_brts <- function(sim, para, mc, with_extinct = FALSE){
  assert_DDvTD_wd()
  assert_para(para)
  assert_sim(sim)
  if(!is.numeric(mc) | !mc %in% 1:1000 ){
    stop('mc must be a numeric in 1:1000')
  }
  if(!is.logical(with_extinct)){
    stop('with_extinct must be a logical')
  }

  trees <- read_sim(sim = sim, para = para)

  if(any(!mc %in% seq_along(trees))){
    stop('There is no tree is the file for that mc.')
  }

  if(!with_extinct){
    brts <- as.numeric( ape::branching.times( trees[[mc]][[1]] ) )
  } else {
    brts <- as.numeric( ape::branching.times( trees[[mc]][[2]] ) )
  }

  rm(trees)

  return(brts)
}
