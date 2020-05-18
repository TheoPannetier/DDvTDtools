#' Run optimisation on trees simulated from empirical phylogenies
#'
#' @param family_name character, the name of the family from which the simulated
#'  trees were generated from. Must match a file in
#'  \code{"DDvTD/data/empirical_trees/"}.
#' @inheritParams params_doc
#' @param outputfile character, the name of the file to save the output data
#' frame in.
#' @param save_results logical. Should the results be saved to
#' \code{outputfile}?
#' @param return_results logical. Should the results be returned?
#' @param jobID SLURM job ID passed when the function is called from a
#' cluster script.
#' @author Théo Pannetier
#' @export

run_optim_empirical_sims <- function(family_name,
                                     sim,
                                     optim,
                                     rangemc = 1:1000,
                                     outputfile = paste0(
                                       family_name, "_sim", sim, "_optim", optim, ".rds"
                                     ),
                                     jobID = NA,
                                     return_results = FALSE,
                                     save_results = TRUE
) {
  assert_DDvTD_wd()

  cat(paste("Loading trees for", family_name, "\n"))
  load(paste0("data/empirical_trees/sim/sim", sim, "_", family_name, ".RData"))

  ## DDD arguments passed to metadata
  optimmethod <- "subplex"
  methode <- "ode45"
  cond <- 1
  num_cycles <- 1
  # Output skeleton
  results <- DDvTDtools::results_optim_struct()

  for (mc in rangemc) {
    cat(paste("\nTree", mc, "/", max(rangemc),"\n"))
    tree <- trees[[mc]][[1]]
    brts <- ape::branching.times(tree)
    N <- ape::Ntip(tree)
    # Initial parameter values = those used to simulate the tree
    # pars come with trees .RData file
    # K is set to N to help convergence
    init_pars <- c(pars[1:2], "K" = N)

    # Call DDD to run ML
    if (optim == "DD") {
      ML_output = try( DDD::dd_ML(
        brts,
        initparsopt = init_pars,
        idparsopt = seq_along(init_pars),
        ddmodel = 1,
        methode = methode,
        optimmethod = optimmethod,
        num_cycles = num_cycles,
        cond = cond
      ))
    } else if (optim == "TD") {
      ML_output = try(DDD::bd_ML(
        brts,
        initparsopt = init_pars + 1E-6,
        idparsopt = seq_along(init_pars),
        tdmodel = 4,
        methode = methode,
        optimmethod = optimmethod,
        num_cycles = num_cycles,
        cond = cond
      ))
    }
    if (!is.data.frame(ML_output)) { # default results in case of an error
      ML_output <- data.frame(
        lambda = NA, mu = NA, K = NA, loglik = -Inf, df = -1, conv = -1
        )
    }
    # Assemble results row for the tree
    results_row <- DDvTDtools::format_optim_results_row(
      mc = mc, sim = sim, optim = optim, brts = brts,
      true_pars = c(age, pars), init_pars = init_pars,
      ML_output = ML_output, methode = methode,
      optimmethod = optimmethod, jobID = jobID,
      num_cycles = num_cycles, cond = cond
    )
    # Append and save
    results <- rbind(results, results_row)
    results <- results[order(results$mc),]
    if (save_results) {
      saveRDS(results, file = paste0("data/empirical_trees/optim/", outputfile))
    }
  }
  if (return_results) {
    return(results)
  }
}









