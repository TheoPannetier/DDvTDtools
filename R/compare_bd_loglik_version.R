#' Compare likelihood values outputed by two versions of bd_loglik() using DDvTD results
#'
#' Reads parameter values in an 'optim" dataset (see DDvTD documentation) and calls \code{bd_loglik()} from \code{DDD.3.2} and \code{3.8}
#'
#' @param sim name of the model used to simulate the trees. See \code{get_sim_names()}
#' @param para four-digit-character specifying the parameters used to simulate the trees. See \code{get_para_values}.
#' @param methode character, optimization methode passed to \code{bd_loglik()}. See \code{DDD} documentation for details.
#' @param sim_dir character, path to the directory where the sim file is located.
#' @param optim_dir character, path to the directory where the optim file is located.
#' @param output_dir character, path to the directory where to save the output.
#'
#' @details \code{DDvTDtools} parameter \code{init} is set to 1 (true value).
#'
#' @author Theo Pannetier
#'
#' @export

compare_bd_loglik_version <- function(sim, para, methode = 'lsoda', sim_dir = "./data/sim/", optim_dir = "./data/optim/", output_dir = "./DDD_loglik_version_comparison/"){
  # Assert input format
  assert_sim(sim)
  assert_para(para)
  if(!is.character(methode)){stop("'methode' should be a character.")}
  if(!is.character(sim_dir)){stop("'sim_dir' should be a character.")}
  if(!is.character(optim_dir)){stop("'optim_dir' should be a character.")}
  if(!is.character(output_dir)){stop("'output_dir' should be a character.")}

  # Set optim parameters
  init = 1 # likelihoods from true-value initialisation
  rangemc = 1:1000

  # Extract branching times from the sim file
  brts_list <- get_multi_brts(input_dir, sim = sim, para = para, rangemc = rangemc)

  # Open 'optim' data frame in the environment
  res <- read_optim_table(optim_dir, sim = sim, optim = optim, init = init, para = para)
  print(paste("Comparing likelihoods using data from sim", sim, "_optimTD_init", init, "-", para, ".rds"))

  # Initialize output data frame
  df <- data.frame(
    "mc" = rep(as.numeric(NA),1000),
    "loglik_optim" = rep(as.numeric(NA),1000),
    "lambda0" = rep(as.numeric(NA),1000),
    "mu0" = rep(as.numeric(NA),1000),
    "K" = rep(as.numeric(NA),1000),
    "loglik_3.2" = rep(as.numeric(NA),1000),
    "loglik_3.8" = rep(as.numeric(NA),1000),
    "diff_version" = rep(as.numeric(NA),1000)
  )

  # Run the comparison on each tree/row
  for(mc in rangemc){
    if(mc %% 100 == 0){
      print(paste("mc =", mc, "..."))
    }
    # extract branching times for this tree
    brts = brts_list[mc]
    # extract parameter values for this row
    pars1 = c(
      res$lambda0[mc],
      res$mu0[mc],
      res$K[mc]
    )
    pars2 = c(
      4, # tdmodel
      1, # cond
      1, # btorph
      0, # print
      2, # soc
      1000 # max lx
    )

    if (any(pars1 %in% c(-1, NA))){ # skip missing / problematic data
      print(paste("No likelihood available - skipping for mc =", mc))
      loglik_diff <- c(pars1,rep(NA,3))
    } else {
      loglik_diff <- llDDD::diff_bd_loglik_version(
        brts = brts,
        pars1 = pars1,
        pars2  = pars2,
        methode = methode
      )
    }

    # Fill output
    output_row <- c(mc, res$loglik[mc], loglik_diff)
    df[mc,] <- output_row
  }
  # Save at destination
  outputfile = paste0("diffLL-", methode, "-", sim, "-TD-", para, ".RData")
  save(df, file = paste0(output_dir, outputfile))
}

