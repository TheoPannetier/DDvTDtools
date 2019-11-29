get_blrt_table <- function(para, init_k) {
  blrt_table <- data.frame(
    lr = numeric(),
    para = factor(levels = rev(get_para_values()), ordered = TRUE),
    sim = factor(levels = get_sim_names(), ordered = TRUE)
  )

  for (sim in get_sim_names()) {
    lr <- get_lr_table(sim = sim, para = para, init_k = init_k)
    subtable <- data.frame(
      lr = lr,
      para = factor(para, levels = rev(get_para_values()), ordered = TRUE),
      sim = factor(sim, levels = get_sim_names(), ordered = TRUE)
    )
    blrt_table <- rbind(blrt_table, subtable)
  }
  blrt_table
}
