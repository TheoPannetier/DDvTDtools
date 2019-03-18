#' Plot the average LTTs of paired DD and TD trees
#'
#' Simultaneously plots the average ltts of a set of DD and TD trees that share the same parameter set.
#'
#' @param para numeric or character. A four-digits code specifying a set of parameter values.
#' @param with_extinct logical. \code{with_extinct = F} for the reconstructed tree, \code{with_extinct = T} for the complete tree.
#'
#' @author Théo Pannetier
#' @export

plot_avg_ltt_DDvTD <- function(para, with_extinct = FALSE){
  assert_DDvTD_wd()
  assert_para(para)
  if(!is.logical(with_extinct)){
    stop('with_extinct must be a logical')
  }
  # Plot average DD ltt
  avg_ltt_DD <- TreeSim::LTT.plot.gen(trees = list(get_sim_multiPhylo("DD", para = para, with_extinct = with_extinct)))[[1]]
  plot(avg_ltt_DD[which(avg_ltt_DD[,2] >= 2),], type = "l", las = 1, col = "green4", xlab = "", ylab = "")

  # Add average TD ltt
  avg_ltt_TD <- TreeSim::LTT.plot.gen(trees = list(get_sim_multiPhylo("TD", para = para, with_extinct = with_extinct)))[[1]]
  lines(avg_ltt_TD[which(avg_ltt_TD[,2] >= 2),], lty = 4, col = "blue")

}
