#' Run maximum likelihood optimisation of a model on simulated trees
#'
#' Read a set of simulated trees as created by [run_sim()], set up initial
#' parameter values, call [DDD::dd_ML()] or [DDD::bd_ML()] to run the
#' optimisation and format output.
#'
#' @inheritParams params_doc
#' @param custom_pars numeric vector, user-specified initial values for the parameters to be optimized.
#' If \code{NULL} (default), values are initialized using true values used to simulate the trees.
#' @param outputfile character, the name of the file to save the output data
#' frame in. The default follows the structure expected by [read_optim_results].
#' @param methode likelihood solving methode, passed to
#' [DDD::dd_ML()] / [DDD::bd_ML()].
#' @param optimmethod optimisation algorithm, passed to
#' [DDD::dd_ML()] / [DDD::bd_ML()].
#' @param tol optimisation tolerance, passed to [DDD::dd_ML()] / [DDD::bd_ML()].
#' @param save_results logical. Should the results be saved to
#' \code{outputfile}?
#' @param return_results logical. Should the results be returned?
#' @param jobID SLURM job ID passed when the function is called from a
#' cluster script.
#' @param num_cycles number of cycles of optimisation,
#' passed to [DDD::dd_ML()] / [DDD::bd_ML()].
#' @param cond code specifying how the likelihood is conditioned. Passed to
#' [DDD::dd_ML()] / [DDD::bd_ML()].
#'
#' @author Théo Pannetier
#'
#' @export
#'
run_optim <- function(
  sim, optim, para, custom_pars = NULL, outputfile = paste0(
    "sim", sim, "_optim", optim, "_", para, "_true_k.rds"
  ), rangemc = 1:1000, methode = "ode45", optimmethod = "subplex",
  tol = rep(1E-6,3), save_results = TRUE, return_results = FALSE,
  jobID = NA, num_cycles = 1, cond = 1
)
{
  # Check argument values format
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_optim(optim)
  assert_para(para)
  if (!is.null(custom_pars) & !(is.numeric(custom_pars))) {
    stop("custom_pars must be a numeric vector")
  }
  if (optim %in% c("DD", "TD")) {
    custom_length <- 3
  } else {
    custom_length <- 2
  }
  if (!is.null(custom_pars) & length(custom_pars) != custom_length) {
    stop(paste("For optim =", optim, "custom_pars must have length", custom_length))
  }
  if (!(is.numeric(rangemc) | is.null(rangemc)) ){stop("rangemc must either be null or a numeric vector.")}
  if (!(is.numeric(rangemc))) {stop("rangemc must be a numeric vector.")}
  if (save_results == TRUE & !is.character(outputfile)) {
    stop("outputfile must be a character")
  }
  if (!is.character(methode)){stop("methode must be a character")}
  if (!is.character(optimmethod)){stop("optimmethod must be a character")}
  if (!is.numeric(tol) | length(tol) != 3 ){
    stop("tol must be a numeric vector of length 3.")
  }
  if (!is.logical(save_results)){stop("save_results must be a logical.")}
  if (!is.logical(return_results)){stop("return_results must be a logical.")}
  if (!(cond %in% 0:3)){stop("cond must be a number between 0 and 3.")}

  # Fetch branch lengths from input file
  cat("Reading trees from input file\n")
  brts_bundle <- get_multi_brts(sim = sim, para = para, rangemc = rangemc)
  # Fetch true parameter values
  true_pars <- para_to_pars(para = para)
  # Set up results data frame
  results <- results_optim_struct()

  for(mc in rangemc){
    cat("\nRunning ML optimisation for tree", mc,"\n")

    brts <- brts_bundle[[which(rangemc == mc)]]
    cat(paste("Nb tips = ", length(brts)+1, "\n"))

    # Set initial parameter values
    if(!is.null(custom_pars)) {
      init_pars <- custom_pars
    } else {
      init_pars <- get_default_initpars(
        true_pars = true_pars, optim = optim, brts = brts
      )
    }
    init_pars <- check_init_pars(
      init_pars = init_pars, optim = optim, brts = brts
    )

    cat(paste("Initial parameter values:"))
    cat(paste(round(init_pars, digits = 2)),"\n")

    # Run maximum likelihood optimisation
    if (optim == "DD") {
      ML_output = try( DDD::dd_ML(
        brts,
        initparsopt = init_pars + 1E-6,
        idparsopt = seq_along(init_pars),
        ddmodel = 1,
        tol = tol,
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
        tol = tol,
        methode = methode,
        optimmethod = optimmethod,
        num_cycles = num_cycles,
        cond = cond
      ))
    }

    if (!is.data.frame(ML_output)){ # default results in case of an error
      ML_output <- data.frame(lambda = NA, mu = NA, K = NA, loglik = -Inf, df = -1, conv = -1)
    }

    # Format output
    results_row <- format_optim_results_row(
      mc = mc, sim = sim, optim = optim, brts = brts,
      true_pars = true_pars, init_pars = init_pars,
      ML_output = ML_output, methode = methode,
      optimmethod = optimmethod, jobID = jobID,
      num_cycles = num_cycles, cond = cond
    )
    results <- rbind(results, results_row)
    results <- results[order(results$mc),]

    if (save_results) {
      saveRDS(results, file = paste0("data/optim/", outputfile))
    }
  }

  if (return_results) {
    return(results)
  }
}
