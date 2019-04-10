#' Plot main features of a simulated tree
#'
#' Plots a simulated tree, along with its ltt and ML estimates for both DD
#' and TD
#'
#' @param sim character, the simulation model.
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values.
#'
#' @return a pdf file saved at \code{DDvTD/figures/} containing a plot for
#' each tree. Each plot contains the plotted tree, the ltt plot, AIC score and
#' parameter estimates of the DD and TD models for this tree.
#'
#' @author Theo Pannetier
#' @export

plot_tree_info <- function(sim, para){
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)

  sim <-  "TD"
  para <- 2241

  # Load data
  trees <- get_sim_multiPhylo(sim = sim, para = para)
  res_DD <- read_optim_table(sim = sim, optim = "DD", para = para)
  res_TD <- read_optim_table(sim = sim, optim = "TD", para = para)

  sim_colour <- ifelse(sim == "DD", "green4", "blue")

  Age <- para_to_pars(para)[1]

  grDevices::pdf(paste0("figures/trees_info_s", sim, "_", para ,".pdf"))
  graphics::par(mar = c(2,2,2,2))
  graphics::layout(matrix(c(1,2,3,3), nrow = 2, ncol = 2, byrow = T))

  for(mc in seq_along(trees)){

    N <- ape::Ntip(trees[[mc]])

    # Plot phylogenetic tree
    ape::plot.phylo(
      trees[[mc]], show.tip.label = F, edge.color = sim_colour,
      main = paste("Tree ", mc)
    )
    # Plot ltt
    ape::ltt.plot(
      phy = trees[[mc]], col = sim_colour,
      main = paste("Age =", Age, " N =", N)
    )
    # Blank plot to put text on
    graphics::plot(
      1:100, 1:100, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
      bty = "n"
    )
    # Plot DD ML results
    graphics::text(
      x = rep(10,5),
      y = seq(90,10, by = -20), adj = 0,
      col = "green4", cex = 1.5,
      labels = c(
        "Results DD:",
        paste("AIC =", round(res_DD$AIC[mc], 2)),
        paste("la0 =", round(res_DD$lambda0_ML[mc], 2)),
        paste("mu0 =", round(res_DD$mu0_ML[mc], 2)),
        paste("K =",   round(res_DD$K_ML[mc], 2))
      )
    )
    graphics::abline(v = c(49,51))
    # Plot TD ML results
    graphics::text(
      x = rep(70,5),
      y = seq(90,10, by = -20), adj = 0,
      col = "blue", cex = 1.5,
      labels = c(
        "Results TD:",
        paste("AIC =", round(res_TD$AIC[mc], 2)),
        paste("la0 =", round(res_TD$lambda0_ML[mc], 2)),
        paste("mu0 =", round(res_TD$mu0_ML[mc], 2)),
        paste("K =",   round(res_TD$K_ML[mc], 2))
      )
    )

  }

  grDevices::dev.off()

  #avg_ltt <- TreeSim::LTT.plot.gen(trees = list(trees))[[1]]
  #graphics::plot(avg_ltt[which(avg_ltt[,2] >= 2),], type = "l", las = 1, xlab = "", ylab = "")
  #graphics::lines(ape::ltt.plot.coords(phy = trees[[mc]]))
}
