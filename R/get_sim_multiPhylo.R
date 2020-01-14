#' Load a simulated dataset as a multiPhylo object
#'
#' @inheritParams params_doc
#'
#' @author Theo Pannetier
#' @export
#'
get_sim_multiPhylo <- function(sim, para, with_extinct = F){
  assert_DDvTD_wd()
  assert_para(para)
  assert_sim(sim)

  if(!is.logical(with_extinct)){
    stop('with_extinct must be a logical')
  }

  trees <- read_trees(sim = sim, para = para)

  for (mc in seq_along(trees)){

    if(!with_extinct){
      # return brts for reconstructed tree
      if(!ape::is.binary.phylo(trees[[mc]][[1]])){
        stop(paste0("Element", mc, "in the list does not contain a binary phylo object."))
      }
      trees[[mc]] <- trees[[mc]][[1]]
    } else{
      # return brts for complete tree
      if(!ape::is.binary.phylo(trees[[mc]][[2]])){
        stop(paste0("Elment", mc, "in the list does not contain a binary phylo object."))
      }
      trees[[mc]] <- trees[[mc]][[2]]
    }
  }
  class(trees) <- "multiPhylo"
  return(trees)
}
