#' Run maximum likelihood esimation of a diversification model on a set of simulated trees
#'
#' \code{run_ML} fetches the branching times from the dataset indicated by
#' \code{sim} and \code{para}, and runs the maximum loglikelihood of the model specified in \code{optim}.
#'
#' @param sim character, the simulation model. Call \code{get_sim_names()} for a list of possible values.
#' @param para character or numeric, the code specifying the parameter values used in the simulation.
#' Call \code{get_para_values()} for possible inputs, and \code{para_to_pars()} for the corresponding parameter values.
#' @param optim character. The name of the model to fit to the simulated phylogenies. See \code{get_optim_names()} for possible inputs.
#' @param custom_pars numeric vector, user-specified initial values for the parameters to be optimized.
#' If \code{NULL} (default), values are initialized using true values used to simulate the trees.
#' @param outputfile character, the name of the file to save the output data frame in.
#' Must be specified if \code{save_results = TRUE}.
#' @param rangemc a numeric vector containing all the indices of the trees to optimize the model on. Default to all the trees in the dataset.
#' @param methode argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more info.
#' @param optimmethod argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more info.
#' @param tol argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more info.
#' @param save_results logical. Should save the results to \code{outputfile} (default) or not.
#' @param return_res logical. Should results be returned? Default to \code{FALSE}.
#' @param jobID is only relevant for metadata. Job scripts pass their ID to this argument. \code{NA} if run locally.
#'
#' @author Théo Pannetier
#'
#' @export
#'
run_ML <- function(sim, optim, para, custom_pars = NULL, outputfile = NULL,
                   rangemc = NULL, methode = "ode45", optimmethod = "subplex",
                   tol = rep(1E-6,3), save_results = TRUE, return_res = FALSE,
                   jobID = NA
)
{
  # Check argument values format
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_optim(optim)
  assert_para(para)
  if (!is.null(custom_pars) & !(is.numeric(custom_pars))){stop("custom_pars must be a numeric vector")}
  if (optim %in% c("DD", "TD")){custom_length <- 3} else {custom_length <- 2}
  if (!is.null(custom_pars) & length(custom_pars) != custom_length){stop(paste("For optim =", optim, "custom_pars must have length", custom_length))}
  if (!(is.numeric(rangemc) | is.null(rangemc)) ){stop("rangemc must either be null or a numeric vector.")}
  if (save_results == TRUE & !is.character(outputfile)){stop("outputfile must be specified")}
  if (!is.character(methode)){stop("methode must be a character")}
  if (!is.character(optimmethod)){stop("optimmethod must be a character")}
  if (!is.numeric(tol) | length(tol) != 3 ){stop("tol must be a numeric vector of length 3.")}
  if (!is.logical(save_results)){stop("save_results must be a logical.")}
  if (!is.logical(return_res)){stop("return_res must be a logical.")}

  # Fetch branch lengths from input file
  cat("Reading trees from input file\n")
  if(is.null(rangemc)){ rangemc <- 1:1000}
  brts_bundle <- get_multi_brts(sim = sim, para = para, rangemc = rangemc)
  # Fetch true parameter values
  true_pars <- para_to_pars(para = para)
  # Set up output data frame
  results_df <- get_empty_optim_df()

  for(mc in rangemc){
    cat("\nRunning ML for tree", mc,"\n")

    brts <- brts_bundle[[which(rangemc == mc)]]
    cat(paste("N = ", length(brts)+1, "\n"))

    # Set initial parameter values
    if(!is.null(custom_pars)){
      init_pars <- custom_pars
    } else {
      init_pars <- get_default_initpars(true_pars = true_pars, optim = optim, brts = brts)
    }
    init_pars <- check_initpars(init_pars = init_pars, optim = optim, brts = brts)
    cat(paste("Initial parameter values:"))
    cat(paste(round(init_pars, digits = 2)),"\n")

    # Run maximum likelihood optimisation
    if (optim == "DD"){
      ML_output = try( DDD::dd_ML(
        brts,
        initparsopt = init_pars + 1E-6,
        idparsopt = seq_along(init_pars),
        tol = tol,
        methode = methode,
        optimmethod = optimmethod
      ))
    } else if(optim %in% c("TD", "CR")){
      if(optim == "TD"){tdmodel = 4} else {tdmodel = 1}
      ML_output = try( DDD::bd_ML(
        brts,
        initparsopt = init_pars + 1E-6,
        idparsopt = seq_along(init_pars),
        tdmodel = tdmodel,
        tol = tol,
        methode = methode,
        optimmethod = optimmethod
      ))
    }
    if(!is.data.frame(ML_output)){ # default results in case of an error
      ML_output <- data.frame(lambda = NA, mu = NA, K = NA, loglik = -Inf, df = -1, conv = -1)
    }

    # Format output
    results_row <- get_optim_df_row(
      mc = mc,
      sim = sim, optim = optim, brts = brts,
      true_pars = true_pars, init_pars = init_pars,
      ML_output = ML_output, methode = methode,
      optimmethod = optimmethod, jobID = jobID
    )

    results_df <- rbind(results_df, results_row)
    results_df <- results_df[order(results_df$mc),]

    assert_DDvTD_wd()
    if(save_results){
      saveRDS(results_df, file = paste0("data/optim/", outputfile))
    }
  }

  if(return_res){
    return(results_df)
  }
}
