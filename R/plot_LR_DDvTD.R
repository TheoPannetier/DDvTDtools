#' Plot logLR distribution for DD & TD trees
#'
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values. See \code{get_para_values()} for possible values.
#' @param init_k character, the setting used to initialise parameter K.
#' See \code{get_possible_init_k()} for possible values.
##'
#' @author Théo Pannetier
#'
#' @export

plot_LR_DDvTD <- function(para, init_k = "true_k"){
  assert_DDvTD_wd()
  assert_para(para)
  assert_init_k(init_k)

  LR_table <- data.frame(
    LR = numeric(),
    para = factor(levels = rev(get_para_values()), ordered = TRUE),
    sim = factor(levels = get_sim_names(), ordered = TRUE)
  )

  for(sim in get_sim_names()){

    LR <- get_LR_DDvTD(sim = sim, para = para, init_k = init_k)
    LR_subtable <- data.frame(
      LR = LR,
      para = factor(para, levels = rev(get_para_values()), ordered = TRUE),
      sim = factor(sim, levels = get_sim_names(), ordered = TRUE)
    )
    LR_table <- rbind(LR_table, LR_subtable)
  }

  gg <- ggplot2::ggplot(
    data = LR_table, ggplot2::aes(x = LR_table$LR, fill = LR_table$sim)
  ) +
    ggplot2::geom_density(alpha = 0.2) +
    ggplot2::scale_fill_manual(values = c("green4", "blue"), guide = FALSE) +
    ggplot2::xlim(min(-5, min(LR)) , max(10, max(LR))) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = bquote(
        "Age"       ~ "=" ~ .(DDvTDtools::para_to_pars(para)[1]) ~ " " ~
        lambda[0]   ~ "=" ~ .(DDvTDtools::para_to_pars(para)[2]) ~ " " ~
        mu[0]       ~ "=" ~ .(DDvTDtools::para_to_pars(para)[3])
      ),
      x = "loglik(DD/TD)"
    )
  gg
}
