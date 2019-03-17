#' Plot the average LTTs of paired DD and TD trees
#'
#' Simultaneously plots the average ltts of a set of DD and TD trees that share the same parameter set.
#'
#' @param para numeric or character. A four-digits code specifying a set of parameter values.
#'
#' @author Théo Pannetier
#' @export

plot_avg_ltt_DDvTD <- function(para){
  assert_DDvTD_wd()
  assert_para(para)
  # Plot average DD ltt
  avg_ltt_DD <- TreeSim::LTT.plot.gen(trees = list(get_sim_multiPhylo("DD", para = para)))[[1]]
  plot(avg_ltt_DD[which(avg_ltt_DD[,2] >= 2),], type = "l", las = 1, col = "green4", xlab = "", ylab = "")

  # Add average TD ltt
  avg_ltt_TD <- TreeSim::LTT.plot.gen(trees = list(get_sim_multiPhylo("TD", para = para)))[[1]]
  lines(avg_ltt_TD[which(avg_ltt_TD[,2] >= 2),], lty = 4, col = "blue")

}
