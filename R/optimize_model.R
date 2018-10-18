#' Optimize a diversification model on a set of simulated phylogenetic trees.
#'
#' Fit a diversification model on pre-existing simulated phylogenetic trees, and returns a dataset containing the optimized parameter values, maximum likelihood estimate and metadata.
#'
#' @param sim_model a character used to identify the desired simulated input dataset. See \code{DDvTD_tags} for a list of possible values.
#' @param para a 4-digit code used to identify the desired simulated input dataset.
#' @param optim_model a character specifying the model to be optimized on input data. See \code{DDvTD_tags} for a list of possible values.
#' @param init an integer controlling the initial parameter values to be used.
#' @param init_pars only used if \code{init = 0}, in which case it should be a numerical vector of appropriate length containing the initial parameter values.
#' @param inputfile path and name of the input file, by default generated automatically from \code{sim_model} and \code{para}
#' @param outputfile path and name for output file, by default generated automatically from \code{sim_model}, \code{para}, \code{optim_model} and \code{init}.
#' @param rangemc a numeric vector containing all the indices of the trees to optimize the model on. Default to all the trees in the dataset.
#' @param overwrite logical. \code{optimize_model} always try to load previous results if they exist. If \code{overwrite} is \code{FALSE} previous results will be kept and trees for which results already exist are excluded from \code{rangemc}.
#' @param methode argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more info.
#' @param optimmethod argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more info.
#' @param tol argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more info.
#' @param cond argument passed to \code{\link[DDD:dd_ML]{dd_ML}} or \code{\link[DDD:dd_ML]{bd_ML}}. See the \pkg{DDD} documentation for more info.
#' @param save_results logical. Should save the results to \code{outpufile} (default) or not.
#' @param return_res logical. Should results be returned? Default to \code{TRUE}.
#'
#' @return A \code{data.frame} including results and metadata:
#' \itemize{
#'    \item{ \code{sim}} the model used to generate simulated data
#'    \item{ \code{ntips}} number of tips at present in the input tree
#'    \item{ \code{crown_age}} crown age or simulation time of the input tree
#'    \item{ \code{true_lambda0}} parameter value used to generate simulated data
#'    \item{ \code{true_mu0}} parameter value used to generate simulated data
#'    \item{ \code{true_K}} parameter used to generate simulated data
#'    \item{ \code{mc}} tree index in the dataset
#'    \item{ \code{df}} number of parameters estimated in the optimization of the model
#'    \item{ \code{optim}} optimized model
#'    \item{ \code{init}} initial parameter values setting
#'    \item{ \code{init_lambda0}} initial parameter value for lambda0
#'    \item{ \code{init_mu0}} initial parameter value for mu0
#'    \item{ \code{init_K}} initial parameter value for K
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

# To test:
# 1 - absence of previous results
# 2 - presence of previous results, no overlap - overwrite on/off
# 3 - presence of previous results, overlap of data - overwrite on/off
# Re-run the optimizations entirely ?


optimize_model <- function(sim_model, para, optim_model, init = 1, init_pars = NULL,
                           inputfile = paste0("./data/sim/","sim",sim_model,"-",
                                              para,".RData"),
                           outputfile = paste0("./data/optim/","sim",sim_model,
                                               "_optim",optim_model,"_init",init,
                                               "-",para,".rds"),
                           rangemc = NULL, overwrite = F, methode = "ode45",
                           optimmethod = "subplex", tol = rep(1E-6,3), cond = 1,
                           save_results = T, return_res = T){

  # Make sure DDD is loaded
  if(!require('DDD')){install.packages('DDD')}
  requireNamespace('DDD') ; requireNamespace('xfun') ; requireNamespace('devtools')

  # Assert arguments formats are correct
  data("DDvTD_tags")
  if( !(sim_model %in% DDvTD_tags & optim_model %in% DDvTD_tags) ){
    stop("Incorrect model specified - see data(DDvTD_tags) for accepted model inputs.")
  }
  data(init_tags)
  if (!(init %in% init_tags) ){stop("init has incorrect value. See data('init_tags') for possible values.")}
  if (!(is.numeric(rangemc) | is.null(rangemc)) ){stop("rangemc must either be null or a numeric vector.")}
  if (!is.logical(overwrite)){stop("overwrite must be either TRUE or FALSE.")}

  # Read inputfile
  cat("Reading input file", inputfile, "\n")
  pars <- NULL
  load(inputfile) # loads 'trees' and 'pars'
  if (is.null(pars)){ pars <- read_para(para)}
  print(pars)
  cat("Results will be saved at", outputfile, "\n")

  # Prepare result data frame
  # Estimates take value -1 by default if no results (i.e. in the case of an error)
  res_template <- data.frame(
    "sim" = factor(sim_model, levels = DDvTD_tags),
    "ntips" = as.numeric(NA),
    "crown_age" = pars[1],
    "true_lambda0" = pars[2],
    "true_mu0" = pars[3],
    "true_K" = pars[4],
    "mc" = as.numeric(NA),# filled below
    "optim" = factor(optim_model, levels = DDvTD_tags),
    "df" = -1,
    "init" = init,
    "init_lambda0" = as.numeric(NA),
    "init_mu0" = as.numeric(NA),
    "init_K" = as.numeric(NA),
    "conv" = as.numeric(NA), # no convergence
    "loglik" = -Inf, # no likelihood estimate -> loglik = 0.
    "AIC" = as.numeric(NA),
    "lambda0" = as.numeric(NA),
    "mu0" = as.numeric(NA),
    "K" = as.numeric(NA)
  )

  # Default results in case of error
  if( optim_model == "DD"){
    outerror <- data.frame(lambda = -1, mu = -1, K = -1, loglik = -Inf,
                           df = -1, conv = -1)
  } else {
    outerror <- data.frame(lambda0 = -1, mu0 = -1, lambda1 = -1, mu1 = -1,
                           loglik = -Inf, df = -1, conv = -1)
  }
  # Default rangemc extend to all trees in the input dataset
  if( is.null(rangemc) ){ rangemc <- seq_along(trees) }

  # Load previous results if they exist
  res = xfun::try_silent(readRDS(outputfile))

  # Initialize the results data frame if no previous results exist
  if( !is.data.frame (res) ){
    res <- NULL
    for(i in 1:1000){ res <- rbind(res,res_template) }
    res$mc <- seq_along(trees)
  }


  # Optimize model for each specified tree in the dataset
  for(mc in rangemc){

    if( c(NULL,res[res$mc == mc,'conv'])[1] %in% c(NA,-1) | overwrite == T ){
      # above vector is a (dirty) trick to evaluate its second element, which can take value integer (O) if no result exist for this mc.
      cat("Optimizing on tree", mc,"\n")
      # Set up initial parameter values
      brts = as.numeric(branching.times(trees[[mc]][[1]]))

      initparsopt <- initialize_pars(pars = pars, para = para, init = init,
                                     init_pars = init_pars, sim_model = sim_model,
                                     optim_model = optim_model, mc = mc, brts = brts)
      # Handle illegal conditions
      if(initparsopt[1] <= initparsopt[2]){
        cat(paste("Illegal values: la0 =",initparsopt[1],"< mu0 =",initparsopt[2],"\n"))
        initparsopt[1] <- initparsopt[1] + 2 * abs(initparsopt[1] - initparsopt[2])
        cat(paste("Changed values to:",initparsopt[1],initparsopt[2]),"\n")
      }
      if(optim_model %in% c("DD","TD") & initparsopt[3] >= 900){
        cat(paste("Illegal value: K =",initparsopt[3],">= 900"),"\n")
        initparsopt[3] <- 500
        cat(paste("Changed value to:",initparsopt[3]),"\n")
      }

      res_mc <- res_template
      res_mc$mc <- mc
      res_mc$ntips <- length(brts) + 1
      res_mc$init_lambda0 <- initparsopt[1]
      res_mc$init_mu0 <- initparsopt[2]
      res_mc$init_K <- initparsopt[3]

      # Optimise the selected model
      cat("Estimating parameters ... ")
      cat(paste(initparsopt,"\n"))

      if (optim_model == "DD"){
        res_temp = try( DDD::dd_ML(brts, initparsopt = initparsopt + 1E-6,
                                   cond = cond, tol = tol, methode = methode,
                                   optimmethod = optimmethod))
      } else if (optim_model == "TD"){
        res_temp = try( DDD::bd_ML(brts, initparsopt = initparsopt + 1E-6,
                                   idparsopt = 1:3, tdmodel = 4, cond = cond,
                                   tol = tol, methode = methode,
                                   optimmethod = optimmethod))
      } else if (optim_model == "CR"){
        res_temp = try( DDD::bd_ML(brts, initparsopt = initparsopt[1:2] + 1E-6,
                                   cond = cond, tol = tol, methode = methode,
                                   optimmethod = optimmethod))
      }

      if(!is.data.frame(res_temp)){
        res_temp <- outerror
      }

      # Organise results
      res_mc$df <- res_temp$df
      res_mc$conv <- res_temp$conv
      res_mc$loglik <- res_temp$loglik
      res_mc$AIC <- 2 * res_temp$df - 2 * res_temp$loglik
      res_mc$lambda0 <- res_temp[,1]
      res_mc$mu0 <-  res_temp[,2]
      res_mc$K <- res_temp[,3] #* 1/(1 - (optim_model == "CR"))

      #res[rangemc,'conv'] %in% c(NA,-1)

      # if an entry already exist for this mc, cut it out
      overlap <- which(res$mc == mc)
      if(length(overlap) > 0){
        res <- res[-overlap,]
      }

      # Then append results and reorder
      res <- rbind(res, res_mc)
      res <- res[order(res$mc),]


    } else {
      cat(paste("Results already found for tree", mc, "\n"))
    }

    # Save
    if (save_results){
      saveRDS(res,outputfile)
    }
  }

  if(return_res){
    return(res)
  }
}
