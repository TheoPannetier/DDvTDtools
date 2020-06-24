#' Compute threshold values and proportions of correct model inference
#'
#' Given a set of parameters, computes the 5th and 95th percentile of the
#' log-likelihood ratio for DD trees and TD trees, respectively, and the
#' proportion of trees of the other distribution with log-likelihood ratio
#' scores beyond those thresholds; i.e., the probability that the right model
#' is recovered from such a tree.
#'
#' @param paras numeric or character vector, a set of `para` values. See
#' [arg_para] for possible values.
#'
#' @return a `tibble` with the parameter values corresponding to `para`, the
#' threshold values based on percentiles, and the proportion of trees beyond
#' the threshold for each model.
#'
#' @author Théo Pannetier
#' @export

get_p_tbl <- function(paras) {

  p_tbl <- purrr::map_dfr(
    .x = paras,
    .f = function(para) {
      optim_tbl <- lapply(arg_sim(), function(sim) {
        join_optim_tbls(sim = sim, para = para) %>%
          dplyr::mutate("sim" = sim)
      }) %>% dplyr::bind_rows()

      threshold_dd <- optim_tbl %>%
        dplyr::filter(sim == "DD") %>%
        dplyr::select(log_lr) %>%
        stats::quantile(probs = 0.05, na.rm = TRUE) %>%
        round(3)

      threshold_td <- optim_tbl %>%
        dplyr::filter(sim == "TD") %>%
        dplyr::select(log_lr) %>%
        stats::quantile(probs = 0.95, na.rm = TRUE) %>%
        round(3)

      p_dd <- optim_tbl %>%
        dplyr::filter(sim == "DD") %>%
        dplyr::summarise(
          "p" = mean(log_lr > threshold_td, na.rm = TRUE) %>%
            round(3)
        ) %>%
        dplyr::pull(p)

      p_td <- optim_tbl %>%
        dplyr::filter(sim == "TD") %>%
        dplyr::summarise(
          "p" = mean(log_lr < threshold_dd, na.rm = TRUE) %>%
            round(3)
        ) %>%
        dplyr::pull(p)

      pars <- para_to_pars(para)

      output <- list(
        "age" = pars[1],
        "la_0" = pars[2],
        "mu_0" = pars[3],
        "K" = pars[4],
        "DD_5th" = threshold_dd,
        "TD_95th" = threshold_td,
        "p_DD" = p_dd,
        "p_TD" = p_td
      )

      return(output)
    })

  return(p_tbl)
}

