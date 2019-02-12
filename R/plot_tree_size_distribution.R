#' Plots the size distribution of trees in a sim dataset
#'
#' Plots the nb of tips at present in the 1,000 trees of the selected tree dataset.
#'
#' @author Theo Pannetier
#'
#' @export

plot_tree_size_distribution <- function(sim, para){

  trees <- read_trees("data/sim/", sim, para)
  tree_size_vector <- c()
  for(mc in seq_along(trees)){
    N <- ape::Ntip(trees[[mc]][[1]])
    tree_size_vector <- c(tree_size_vector,N)
  }

  if(sim == "DD"){
    col = "green"
  } else if(sim == "TD"){
    col = "blue"
  } else {
    col = "NA"
  }
  breaks <- (1:(max(tree_size_vector)/2))*2
  hist(
    x = tree_size_vector,
    breaks = breaks,
    main = paste0("sim", sim, "-", para),
    xlab = "N",
    col = col
    )
  K = para_to_pars(para)[4]
  abline(v = K, col = "red")

}
