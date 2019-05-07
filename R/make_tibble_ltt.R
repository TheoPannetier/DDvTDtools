make_tibble_ltt <- function(phylo) {
  if (!(ape::is.binary.phylo(phylo))) {
    stop("'phylo' must be a binary phylogenetic tree")
  }
  dplyr::tibble(
    time = ape::ltt.plot.coords(phylo)[,1], 
    N = ape::ltt.plot.coords(phylo)[,2]
  )
}
