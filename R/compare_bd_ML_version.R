#' Compare likelihood values outputed by two versions of bd_ML() using DDvTD results
#'
#' Assembles a data frame containing ML estimates from bd_ML, bd_ML_3.8, and previous results from DDvTD.
#'
#' @param sim name of the model used to simulate the trees. See \code{get_sim_names()}
#' @param para four-digit-character specifying the parameters used to simulate the trees. See \code{get_para_values}.
#'
#' @details \code{DDvTDtools} parameter \code{init} is set to 1 (true value). The comparison is run on the 50 first trees.
#'
#' @author Theo Pannetier
#'
#' @export

compare_bd_ML_version <- function(sim, para){

  rangemc = 1:50
  init = 1

  idparsopt = 1:3
  missnumspec = 0
  tdmodel = 4
  cond = 1
  btorph = 1
  soc = 2
  optimmethod = "subplex"
  methode = "lsoda"

  df <- data.frame(
    "mc" = rep(as.numeric(NA),length(rangemc)),
    "ML_previous" = rep(as.numeric(NA),length(rangemc)),
    "ML_3.2" = rep(as.numeric(NA),length(rangemc)),
    "diff_previous_3.2" = rep(as.numeric(NA),length(rangemc)),
    "ML_3.8" = rep(as.numeric(NA),length(rangemc)),
    "diff_version" = rep(as.numeric(NA),length(rangemc)),
    "lambda0_previous" = rep(as.numeric(NA),length(rangemc)),
    "lambda0_3.2" = rep(as.numeric(NA),length(rangemc)),
    "lambda0_3.8" = rep(as.numeric(NA),length(rangemc)),
    "mu0_previous" = rep(as.numeric(NA),length(rangemc)),
    "mu0_3.2" = rep(as.numeric(NA),length(rangemc)),
    "mu0_3.8" = rep(as.numeric(NA),length(rangemc)),
    "K_previous" = rep(as.numeric(NA),length(rangemc)),
    "K_3.2" = rep(as.numeric(NA),length(rangemc)),
    "K_3.8" = rep(as.numeric(NA),length(rangemc))
  )
  outputfile = paste0("diffML-", sim, "-TD-", para, ".RData")

  previous <- read_optim_table(dir = "data/optim/", sim = sim, optim = "TD", init = init, para = para)
  pars <- para_to_pars(para)[2:4]

  for(mc in rangemc){

    print(paste("Running likelihoods for tree ", mc))

    # Load data
    brts <- get_brts(dir = "data/sim/", sim = sim, para = para, mc = mc)

    # Fill previous data entries
    df$mc[mc] = mc
    df$ML_previous[mc] = previous$loglik[mc]
    df$lambda0_previous[mc] = previous$lambda0[mc]
    df$mu0_previous[mc] = previous$mu0[mc]
    df$K_previous[mc] = previous$K[mc]

    ML_3.2 <- llDDD::bd_ML_3.2(
      brts = brts,
      initparsopt = pars,
      idparsopt = idparsopt,
      missnumspec = missnumspec,
      tdmodel = tdmodel,
      cond = cond,
      btorph = btorph,
      soc = soc,
      optimmethod = optimmethod
    )

    df$ML_3.2[mc] <- ML_3.2$loglik
    df$lambda0_3.2[mc] <- ML_3.2$lambda0
    df$mu0_3.2[mc] <- ML_3.2$mu0
    df$K_3.2[mc] <- ML_3.2$lambda1

    ML_3.8 <- DDD::bd_ML(
      brts = brts,
      initparsopt = pars,
      idparsopt = idparsopt,
      missnumspec = missnumspec,
      tdmodel = tdmodel,
      cond = cond,
      btorph = btorph,
      soc = soc,
      optimmethod = optimmethod
    )

    df$ML_3.8[mc] <- ML_3.8$loglik
    df$lambda0_3.8[mc] <- ML_3.8$lambda0
    df$mu0_3.8[mc] <- ML_3.8$mu0
    df$K_3.8[mc] <- ML_3.8$lambda1

    if(!any(c(df$ML_previous[mc], df$ML_3.2[mc]) %in% c(NA, -Inf, -1) )){
      df$diff_previous_3.2[mc] = df$ML_previous[mc] - df$ML_3.2[mc]
    }
    if(!any(c(df$ML_3.2[mc], df$ML_3.8[mc]) %in% c(NA, -Inf, -1) )){
      df$diff_version[mc] = df$ML_3.2[mc] - df$ML_3.8[mc]
    }
    save(df, file = paste0("DDD_loglik_version_comparison/", outputfile))
  }

}
