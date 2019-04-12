#' Compute likelihood landscape along K for a range of trees
#' 
#' Calls \code{get_likelihood_along_K()} for a range of trees in DDvTD
#' 
#' @param sim character, the name of a simulation model. See 
#' \code{get_sim_names()} for possible values.
#' @param optim character. The name of the model to get the likelihood landscape
#' of, can be either "DD" or "TD".
#' @param para numeric or character. A four-digits code specifying a set of 
#' parameter values. See \code{get_para_values()}
#' @param fixed_pars_values character specifying which values to use for lambda0
#' and mu0. Either 'true_values', using true values of the parameters given by
#' \code{para_to_pars()}; or 'ML_estimates', using maximum likelihood estimates
#' fetched from the ML results dataset stored in \code{data/optim/}.
#' @param outputfile character, the path and filename to save results at.
#' 
#' @author Théo Pannetier
#' @export

run_liklandscapes <- function(sim, 
                              optim, 
                              para, 
                              fixed_pars_values = "true_values",
                              outputfile = paste0(
                                "data/lik_landscapes/lik_landscape_",
                                "s", sim, "_o", optim, "_", para,
                                "_", fixed_pars_values, ".rds"
                              )
) {
  # Check input ----------------------------------------------------------------
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_optim(optim)
  if (optim == "CR") {
    stop("function not intended for optim = CR. Please choose either DD or TD.")
  }
  assert_para(para)
  if (!(fixed_pars_values) %in% c("true_values", "ML_estimates")) {
    stop(
      "Incorrect input for fixed_pars_values:
       pick either 'true_values' or 'ML_estimates'"
    )
  }
  if (!is.character(outputfile)){
    stop("outputfile must be a character.")
  }
  
  rangemc <- seq(1, 1000, by = 50)

  # Initialise results data frame ----------------------------------------------
  results <- data.frame(
    "mc" = numeric(),
    "lambda0" = numeric(),
    "mu0" = numeric(),
    "K" = numeric(),
    "loglik" = numeric(),
    "likelihood" = numeric()
  )
  
  for (mc in rangemc) {
    cat("Running for tree", mc, "\n")
    brts <- get_brts(sim = sim, para = para, mc = mc)
    ntips <- length(brts) + 1
    
    # Assemble the K sequence --------------------------------------------------
    K_seq <- c(seq(10, 100, by = 10), c(seq(100, 1000, by = 100))) 
    zoom_around_N <- seq(from = max(5, ntips - 10), to = ntips + 10, by = 1)
    zoom_around_K <- seq(30, 50, by = 1)
    K_seq <- sort(unique(c(K_seq, zoom_around_K, zoom_around_N)))
    
    # Set lambda0 & mu0 values -------------------------------------------------
    if (fixed_pars_values == "ML_estimates") {
      
      ML_results <- DDvTDtools::read_optim_table(
        sim = sim,
        optim = optim,
        para = para
      )[mc,]
      
      if (ML_results$hasConverged == FALSE) {
        warning(
          "No ML estimate found for this tree. Skipping to next tree."
        )
        next()
      }
      lambda0 <- ML_results$lambda0_ML
      mu0 <- ML_results$mu0_ML
      
    } else if (fixed_pars_values == "true_values") {
      lambda0 <- para_to_pars(para)[2]
      mu0 <- para_to_pars(para)[3]
    }
    
    # Get loglik landscape along K_seq -----------------------------------------
    liklandscape <- DDvTDtools::get_likelihood_along_K(
      optim = optim,
      brts = brts,
      lambda0 = lambda0,
      mu0 = mu0,
      K_seq = K_seq
    )
    
    # Format and save output ---------------------------------------------------
    liklandscape <- cbind("mc" = mc, liklandscape)
    results <- rbind(results, liklandscape)
    saveRDS(results, outputfile)
  }
  
  
}

