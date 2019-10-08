#' Run maximum likelihood esimation starting from the number of lineages
#'
#' \code{run_from_n} is simply a wrapper of \code{run_ML} where K is initialized
#' from the number of tips in the tree, rather than the true value.
#'
#' @param sim character, the simulation model. Call \code{get_sim_names()} for a
#'  list of possible values.
#' @param para character or numeric, the code specifying the parameter values
#' used in the simulation.
#' Call \code{get_para_values()} for possible inputs, and \code{para_to_pars()}
#' for the corresponding parameter values.
#' @param optim character. The name of the model to fit to the simulated
#' phylogenies. See \code{get_optim_names()} for possible inputs.
#' If \code{NULL} (default), values are initialized using true values used to
#' simulate the trees.
#' @param rangemc a numeric vector containing all the indices of the trees to
#' optimize the model on. Default to all the trees in the dataset.
#' @param methode argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or
#' \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more
#' info.
#' @param optimmethod argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or
#' \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more
#' info.
#' @param tol argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or
#' \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more
#' info.
#' @param save_results logical. Should save the results to \code{outputfile}
#' (default) or not.
#' @param return_res logical. Should results be returned? Default to
#' \code{FALSE}.
#' @param jobID is only relevant for metadata. Job scripts pass their ID to
#' this argument. \code{NA} if run locally.
#' @param num_cycles numeric, passed to \code{dd_ML}/\code{bd_ML}. Number of
#' cycles of optimisation.
#' @param cond numeric, conditionning parameter passed to
#' \code{dd_ML}/\code{bd_ML}. See \code{?dd_ML} for possible values.

#' @author Théo Pannetier
#'
#' @export
#'
run_from_n <- function(sim, optim, para, rangemc = NULL,
                       methode = "ode45", optimmethod = "subplex",
                       tol = rep(1E-6,3), jobID = NA, num_cycles = 1,
                       save_results = TRUE, return_res = FALSE, cond = 1) {
  # Check argument values format
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_optim(optim)
  assert_para(para)

  if (!(is.numeric(rangemc) | is.null(rangemc))) {
    stop("rangemc must either be null or a numeric vector.")
    }
  if (!is.character(methode)){stop("methode must be a character")}
  if (!is.character(optimmethod)){stop("optimmethod must be a character")}
  if (!is.logical(save_results)){stop("save_results must be a logical.")}
  if (!is.logical(return_res)){stop("return_res must be a logical.")}
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
  if (is.null(rangemc)) {
    rangemc <- 1:1000
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
      row <- run_ML(
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
        return_res = TRUE,
        jobID = jobID,
        num_cycles = num_cycles,
        cond = cond
      )
      cat("\n")
      row
    }
  )

  df <- get_empty_optim_df()
  for(i in seq_along(res)) {
    df <- rbind(df, res[[i]])
  }
  if(save_results){
    saveRDS(df, file = paste0("data/optim/", outputfile))
  }
  if(return_res){
    return(df)
  }
}
