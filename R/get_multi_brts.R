#' Extract multiple sets of branching times from a dataset of simulated trees
#'
#' Read the dataset at specified location and returns branching times for
#' multiple trees specified by indices.
#'
#' @inheritParams params_doc
#'
#' @return a list of numerical vectors containing the branching times of the specified trees.
#'
#' @author Theo Pannetier
#' @export

get_multi_brts <- function(sim, para, rangemc = NULL, with_extinct = FALSE) {
  assert_DDvTD_wd()
  assert_para(para)
  assert_sim(sim)
  if ( is.null(rangemc)) {rangemc <- 1:1000}
  if (!is.numeric(rangemc) | any(!rangemc %in% 1:1000 )) {
    stop('mc must be a vector of numeric values between 1 and 1000')
  }
  if (!is.logical(with_extinct)){
    stop('with_extinct must be a logical')
  }

  brts_bundle = list()

  trees <- read_sim(sim = sim, para = para)

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
