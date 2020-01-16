#' Run maximum likelihood esimation starting from the number of lineages
#'
#' An alternative version of [run_optim()] where the initial value of K is set
#' to N, the nb of tips in the trees, in order to ease convergence to the
#' maximum likelihood.
#'
#' @inheritParams params_doc
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

#' @author Théo Pannetier
#'
#' @export
#'
run_optim_from_n <- function(sim, optim, para, rangemc = 1:1000,
                       methode = "ode45", optimmethod = "subplex",
                       tol = rep(1E-6,3), jobID = NA, num_cycles = 1,
                       save_results = TRUE, return_results = FALSE, cond = 1) {
  # Check argument values format
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_optim(optim)
  assert_para(para)

  if (!is.numeric(rangemc)) {
    stop("rangemc must be a numeric vector.")
    }
  if (!is.character(methode)){stop("methode must be a character")}
  if (!is.character(optimmethod)){stop("optimmethod must be a character")}
  if (!is.logical(save_results)){stop("save_results must be a logical.")}
  if (!is.logical(return_results)){stop("return_results must be a logical.")}
  if (!is.numeric(tol) | length(tol) != 3 ){
    stop("tol must be a numeric vector of length 3.")}
  if (is.na(jobID)) {
    outputfile <- paste0(
      "sim", sim, "_optim", optim, "_", para, "_from_n.rds"
    )
  } else {
    outputfile <- paste0(
      "sim", sim, "_optim", optim, "_", para, "_from_n_", jobID, ".rds"
    )
  }
  if (!(cond %in% 0:3)){stop("cond must be a number between 0 and 3.")}

  res <- lapply(
    X = rangemc,
    FUN = function(x) {
      mc <- x
      cat("\nRunning ML for tree", mc,"\n")
      n <- length(get_brts(sim = sim, para = para, mc = mc)) + 1
      custom_pars <- c(
        "lambda0" = para_to_pars(para)[2],
        "mu0" = para_to_pars(para)[3],
        "k" = n
        )
      row <- run_optim(
        sim = sim,
        optim = optim,
        para = para,
        custom_pars = custom_pars,
        outputfile = outputfile,
        rangemc = mc,
        methode = methode,
        optimmethod = optimmethod,
        tol = tol,
        save_results = FALSE,
        return_results = TRUE,
        jobID = jobID,
        num_cycles = num_cycles,
        cond = cond
      )
      cat("\n")
      row
    }
  )

  results <- results_optim_struct()
  for(i in seq_along(res)) {
    results <- rbind(results, res[[i]])
  }
  if(save_results){
    saveRDS(results, file = paste0("data/optim/", outputfile))
  }
  if(return_results){
    return(results)
  }
}
