#' Plot likelihood landscapes along K
#'
#' Plot likelihood curves of DD and TD models over parameter K from saved
#' results.
#'
#' @param sim character, the name of a simulation model. See
#' \code{get_sim_names()} for possible values.
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values.
#'
#' @author Théo Pannetier
#' @export

plot_loglik_landscape_K <- function(sim, para) {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)

  data_DD_true <- readRDS(paste0(
    "data/lik_landscapes/lik_landscape",
    "_s", sim, "_oDD", "_", para,
    "_true_values.rds"
  ))
  data_DD_true <- cbind(
    data_DD_true[, -c(2, 3)],
    "fixed_pars" = factor(
      "true_values",
      levels = c("true_values", "ML_estimates")
    )
  )
  data_DD_ML <- readRDS(paste0(
    "data/lik_landscapes/lik_landscape",
    "_s", sim, "_oDD", "_", para,
    "_ML_estimates.rds"
  ))
  data_DD_ML <- cbind(
    data_DD_ML[, -c(2, 3)],
    "fixed_pars" = factor(
      "ML_estimates",
      levels = c("true_values", "ML_estimates")
    )
  )
  data_DD <- rbind(data_DD_true, data_DD_ML)

  data_TD_true <- readRDS(paste0(
    "data/lik_landscapes/lik_landscape",
    "_s", sim, "_oTD", "_", para,
    "_true_values.rds"
  ))
  data_TD_true <- cbind(
    data_TD_true[, -c(2, 3)],
    "fixed_pars" = factor(
      "true_values",
      levels = c("true_values", "ML_estimates")
    )
  )
  data_TD_ML <- readRDS(paste0(
    "data/lik_landscapes/lik_landscape",
    "_s", sim, "_oTD", "_", para,
    "_ML_estimates.rds"
  ))
  data_TD_ML <- cbind(
    data_TD_ML[, -c(2, 3)],
    "fixed_pars" = factor(
      "ML_estimates",
      levels = c("true_values", "ML_estimates")
    )
  )
  data_TD <- rbind(data_TD_true, data_TD_ML)

  true_K <- para_to_pars(para)[4]

  rangemc <- unique(data_DD$mc)
  for (mc in rangemc) {

    N <- length(get_brts(sim = sim, para = para, mc = mc)) + 1

    lik_landscape <- rbind(
      cbind(
        "optim" = factor("DD", levels = get_optim_names()),
        data_DD[data_DD$mc == mc, ]
      ),
      cbind(
        "optim" = factor("TD", levels = get_optim_names()),
        data_TD[data_DD$mc == mc, ]
      )
    )
    # Translate -Inf's into NAs
    lik_landscape$loglik[which(lik_landscape$loglik == -Inf)] <- NA

    x <- ggplot2::ggplot(
      data = lik_landscape,
      ggplot2::aes(
        x = K,
        y = loglik
      )
    )
    x + ggplot2::geom_line(
      ggplot2::aes(
        colour = optim,
        linetype = fixed_pars
      )
    ) +
      ggplot2::scale_colour_manual(
        values = c("green4", "blue")
      ) +
      ggplot2::geom_vline(
        xintercept = true_K,
        linetype = "longdash",
        color = "grey70"
      ) +
      ggplot2::geom_vline(
        xintercept = N,
        linetype = "longdash",
        color = "purple"
      ) +
      ggplot2::labs(
        x = "K",
        y = "logL"
      ) +
      ggplot2::scale_fill_discrete(labels = c("A", "B", "C", "D"))
  }
}


