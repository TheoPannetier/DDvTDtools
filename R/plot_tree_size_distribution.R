#' Plots the size distribution of trees in a sim dataset
#'
#' Plots the nb of tips at present in the 1,000 trees of the selected tree dataset.
#'
#' @author Theo Pannetier
#'
#' @export

plot_tree_size_distribution <- function(sim){

  trees <- read_trees(sim, para)
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
    #breaks = breaks,
    main = paste0("sim", sim, "-", para),
    xlab = "N",
    xlim = c(1,max(tree_size_vector)*1.2),
    col = col
    )
  K = para_to_pars(para)[4]
  abline(v = K, col = "red")
# ########
#   all_para = get_para_values()
#   tree_sizes = data.frame(N = numeric(), para = factor(levels = all_para))
#   # Fill dataset with sizes of trees
#   for(one_para in all_para){
#     trees <- read_trees("data/sim/", sim, one_para)
#     for(mc in seq_along(trees)){
#       N <- ape::Ntip(trees[[mc]][[1]])
#       tree_sizes <- rbind(
#         tree_sizes,
#         data.frame(N = N, para = factor(one_para, levels = all_para))
#       )
#     }
#   }
#   K = para_to_pars(para)[4]
#
#
#   ggplot(data = tree_sizes, aes(x = para, y = N, fill = para)) +
#     geom_violin(trim = FALSE) +
#     geom_hline(yintercept = K)
#     # scale_fill_manual(values = c("darkgreen", "red4")) +
#     # ggtitle("DAISIE extintion rate estimates in ontogeny \nand null-ontogeny scenarios")  +
#     ylab("Extinction rate estimates (log transformed)") +
#     xlab(element_blank()) +
#     theme(
#       axis.ticks.x = element_blank(),
#       legend.position = "none",
#       axis.line = element_line("black"),
#       panel.background = element_blank()
#     )
#     #scale_x_discrete(labels=c("island_ontogeny" = "Island ontogeny", "no_ontogeny" = "Null-ontogeny")) +
#     # geom_errorbar( # Add horizontal bars in set postion (specify in hlines_df)
#     #   data = hlines_df,
#     #   aes(y = NULL, ymax = hline, ymin = hline),
#     #   color = "orange2", size = 1.1
#     # )

}
