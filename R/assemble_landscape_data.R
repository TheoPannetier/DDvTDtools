#' Assemble log-likelihood landscape data for plotting
#'
#' Fetches saved results for likelihood landscapes along K and assembles a data
#' frame ready for plotting
#'
#' @param sim character, the name of a simulation model. See
#' \code{get_sim_names()} for possible values.
#' @param para numeric or character. A four-digits code specifying a set of
#' parameter values.
#' @param mc numeric, the index of the tree to get the landscape for.
#'
#' @author Théo Pannetier
#' @export

assemble_landscape_data <- function(sim, para, mc) {
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  if (!is.numeric(mc)) {
    stop("mc must be a numeric")
  }

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

  if (!(mc %in% data_DD$mc) | !(mc %in% data_TD$mc)) {
    stop("chosen mc absent from the data.")
  }

  landscape_data <- rbind(
    cbind(
      "optim" = factor("DD", levels = get_optim_names()),
      data_DD[data_DD$mc == mc, ]
    ),
    cbind(
      "optim" = factor("TD", levels = get_optim_names()),
      data_TD[data_DD$mc == mc, ]
    )
  )
  landscape_data$loglik[which(landscape_data$loglik == -Inf)] <- NA
  landscape_data
}
