#' Plot simulated trees with ML parameter estimates
#'
#' Plots the simulated trees in the dataset with paramter estimated with both
#' the DD and the TD model under each tree.
#'
#' @param sim name of the model used to simulate the trees. See \code{get_sim_names()}
#' @param para four-digit-character specifying the parameters used to simulate the trees. See \code{get_para_values}.
#' @param rangemc numeric vector, indexes of the trees to be plotted
#'
#' @export
#'
#' @author Théo Pannetier
#'
plot_trees_estimates <- function(sim, para, rangemc = 1:1000){
  assert_sim(sim)
  assert_para(para)
  assert_DDvTD_wd()
  if(!is.numeric(rangemc)){stop("argument rangemc should be numeric")}

  trees <- get_sim_multiPhylo(sim = sim, para = para)
  res_DD <- read_optim_table(sim = sim, optim = "DD", init = 1, para = para)
  res_TD <- read_optim_table(sim = sim, optim = "TD", init = 1, para = para)

  if(sim == "DD"){tree_colour <- "green3"} else {tree_colour <- "blue"}
  if(optim == "DD"){text_colour <- "green3"} else {text_colour <- "blue"}

  outputfile <- paste0("figures/", "trees_estimates_", "sim", sim, "-", para, ".pdf")
  print(paste("Plots saved under", getwd(), outputfile))

  pdf(file = outputfile, paper = "a4")
  par(mfrow = c(5, 5), mar = c(5,1,1,1))
  for(mc in rangemc){

    estimates_legend <- paste0(
      "ntips = ", ape::Ntip(trees[[mc]]),
      "\n    DD", "   ", "TD",
      "\nla0  ", round(res_DD$lambda0[mc], 2),"  ", round(res_TD$lambda0[mc], 2),
      "\nmu0  ", round(res_DD$mu0[mc], 2),"  ", round(res_TD$mu0[mc], 2),
      "\nK  ", round(res_DD$K[mc], 2),"  ", round(res_TD$K[mc], 2)
    )
    ape::plot.phylo(
      trees[[mc]],
      show.tip.label = F,
      edge.color = tree_colour,
      sub = estimates_legend
    )
  }
  dev.off()
}
