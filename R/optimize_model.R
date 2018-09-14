#' Optimize a diversification model on a set of simulated phylogenetic trees.
#'
#' Fit a diversification model on pre-existing simulated phylogenetic trees, and returns a dataset containing the optimized parameter values, maximum likelihood estimate and metadata.
#'
#' @param sim_model a character used to identify the desired simulated input dataset. See \code{DDvTD_tags} for a list of possible values.
#' @param para a 4-digit code used to identify the desired simulated input dataset.
#' @param optim_model a character specifying the model to be optimized on input data. See \code{DDvTD_tags} for a list of possible values.
#' @param init an integer controlling the initial parameter values to be used.
#' @param inputfile path and name of the input file, by default generated automatically from \code{sim_model} and \code{para}
#' @param outputfile path and name for output file, by default generated automatically from \code{sim_model}, \code{para}, \code{optim_model} and \code{init}.
#' @param rangemc a numeric vector containing all the indices of the trees to optimize the model on. Default to all the trees in the dataset.
#' @param overwrite logical. \code{optimize_model} always try to load previous results if they exist. If \code{overwrite} is \code{FALSE} previous results will be kept and trees for which results already exist are excluded from \code{rangemc}.
#' @param ... additional parameter values to be passed to \code{dd_ML} or \code{bd_ML}, e.g. \code{cond}, \code{tol} or \code{methode}.
#' @param methode argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more info.
#' @param optimmethod argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more info.
#' @param tol argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more info.
#' @param cond argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more info.
#'
#' @return A \code{data.frame} including results and metadata:
#' \itemize{
#'    \item{ \code{sim}} the model used to generate simulated data
#'    \item{ \code{true_lambda0}} parameter value used to generate simulated data
#'    \item{ \code{true_mu0}} parameter value used to generate simulated data
#'    \item{ \code{true_K}} parameter used to generate simulated data
#'    \item{ \code{mc}} tree index in the dataset
#'    \item{ \code{df}} number of parameters estimated in the optimization of the model
#'    \item{ \code{optim}} optimized model
#'    \item{ \code{init}} initial parameter values setting
#'    \item{ \code{conv}} exit status of the optimization. conv = 0 means convergence.
#'    \item{ \code{loglik}} log-likelihood estimate associated with optimized parameter values below
#'    \item{ \code{AIC}} AIC value calculated from \code{loglik} and \code{df}
#'    \item{ \code{lambda0}} maximum likelihood estimate for lambda0
#'    \item{ \code{mu0}} maximum likelihood estimate for mu0
#'    \item{ \code{K}} maximum likelihood estimate for K. Takes value \code{Inf} for \code{optim_model = "CR"}.
#' }
#'
#' @author Theo Pannetier, \email{t.s.c.pannetier@rug.nl}
#'
#' @export


optimize_model <- function(sim_model, para, optim_model, init = 1,
                           inputfile = paste0("./data/sim/","sim",sim_model,"-",para,".RData"),
                           outputfile = paste0("./data/optim/","sim",sim_model,"_optim",optim_model,"_init",init,"-",para,".rds"),
                           rangemc = NULL, overwrite = F, methode = "ode45", optimmethod = "subplex",
                           tol = rep(1E-6,3), cond = 1){
  # Make sure DDD is loaded
  if(!require('DDD')){install.packages('DDD')}
  requireNamespace('DDD') ; requireNamespace('xfun') ; requireNamespace('devtools')

  # Assert arguments are correct
  check_model_tag(sim_model) ; check_model_tag(optim_model) ; check_init(init)
  if (!(is.numeric(rangemc) | is.null(rangemc)) ){stop("rangemc must either be null or a numeric vector.")}
  if (!is.logical(overwrite)){stop("overwrite must be either TRUE or FALSE.")}

  # Read parameter values
  pars <- read_para(para)

  # Read inputfile
  cat("Reading input file", inputfile, "\n")
  load(inputfile)

  # Default rangemc extend to all trees in the input dataset
  if( is.null(rangemc) ){ rangemc <- seq_along(trees) }

  # Load previous results if they exist
  prev_res = xfun::try_silent(readRDS(outputfile))

  # Exclude existing results from rangemc if overwrite is off
  if( class(prev_res) == "data.frame" ){

    mc_overlap <- rangemc[which(rangemc %in% prev_res$mc)]

    if(length(mc_overlap) > 0) {

      cat("Previous results found for the following trees: ", mc_overlap, "\n")
      if(overwrite){
        cat("Previous results will be overwritten.\n")
      } else {
        cat ("Previous results will not be overwritten.\n")
        rangemc <- rangemc[-which(rangemc %in% mc_overlap)] }
      }
  }

  # Declare results storage dataset
  res_temp <- NULL

  # Default results in case of error
  if( optim_model == "DD"){
    outerror <- data.frame(lambda = -1, mu = -1, K = -1, loglik = -Inf, df = -1, conv = -1)
  } else {
    outerror <- data.frame(lambda0 = -1, mu0 = -1, lambda1 = -1, mu1 = -1, loglik = -Inf, df = -1, conv = -1)
  }

  # Optimize model for each specified tree in the dataset
  for(mc in rangemc){

    cat("Optimizing on tree", mc,"\n")

    # Set up initial parameter values
    brts = as.numeric(branching.times(trees[[mc]][[1]]))
    initpars <- read_init_dd(pars, init)

    flush.console()

    # Optimise the selected model
    cat("Estimating parameters ... ")
    if (optim_model == "DD"){
      res_mc = try( DDD::dd_ML(brts, initparsopt = initpars[2:4] + 1E-6, cond = cond, tol = tol, methode = methods, optimmethod = optimmethod))
    } else if (optim_model == "TD"){
      res_mc = try( DDD::bd_ML(brts, initparsopt = initpars[2:4] + 1E-6, idparsopt = 1:3, tdmodel = 4, cond = cond, tol = tol, methode = methode, optimmethod = optimmethod))
    } else if (optim_model == "CR"){
      res_mc = try( DDD::bd_ML(brts, initparsopt = initpars[2:3] + 1E-6, cond = cond, tol = tol, methode = methode, optimmethod = optimmethod))
    }

    # Organise results
    if(!is.data.frame(res_mc)) { res_mc = outerror } # default dataset if error
    res_mc <- cbind(res_mc,mc)                       # add tree index
    res_temp <- rbind(res_temp, res_mc)              # append to result dataset
  }

  # Tidy dataset format
  data("DDvTD_tags")

  if (is.null(res_temp)){
    res <- res_temp }
  else {
    res <- data.frame(
      "sim" = factor(sim_model, levels = DDvTD_tags),
      "true_lambda0" = pars[2],
      "true_mu0" = pars[3],
      "true_K" = pars[4],
      "mc" = res_temp$mc,
      "optim" = factor(optim_model, levels = DDvTD_tags),
      "df" = res_temp$df,
      "init" = init,
      "conv" = res_temp$conv,
      "loglik" = res_temp$loglik,
      "AIC" = - 2*res_temp$loglik + 2*res_temp$df,
      "lambda0" = res_temp[,1],
      "mu0" = res_temp[,2],
      "K" = res_temp[,3] * (optim_model != "CR") + Inf * (optim_model == "CR")
    )
  }

  # Assemble new results with previous ones
  if(class(prev_res) == "data.frame" ){
    if(length(mc_overlap > 0) & overwrite){
      res <- rbind(res, prev_res[-which(prev_res$mc %in% mc_overlap),] )
    } else { res <- rbind(res, prev_res) }
  }
  res <- res[order(res$mc),]

  cat("Saving at", outputfile)
  saveRDS(res,outputfile)
  return(res)
}
