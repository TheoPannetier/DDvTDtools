#' Plot logLR distribution for DD & TD trees
#'
#' @param para numeric or character. A four-digits code specifying a set of parameter values. See \code{get_para_values()} for possible values.
#'
#' @author Théo Pannetier
#'
#' @export

plot_LR_DDvTD <- function(para){
  assert_DDvTD_wd()
  assert_para(para)

  LR_table <- data.frame(
    LR = numeric(),
    para = factor(levels = rev(get_para_values()), ordered = TRUE),
    sim = factor(levels = get_sim_names(), ordered = TRUE)
  )

  for(sim in get_sim_names()){

    LR <- get_LR_DDvTD(sim = sim, para = para)
    LR_subtable <- data.frame(
      LR = LR,
      para = factor(para,levels = rev(get_para_values()), ordered = TRUE),
      sim = factor(sim, levels = get_sim_names(), ordered = TRUE)
    )
    LR_table <- rbind(LR_table, LR_subtable)
  }

  gg <- ggplot2::ggplot(data = LR_table, ggplot2::aes(x = LR, fill = sim)) +
    ggplot2::geom_density(alpha=.2) +
    ggplot2::scale_fill_manual(values = c("green4", "blue"), guide = FALSE)
  #ggplot2::ylim(-8,8) +
  #ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  gg
}

