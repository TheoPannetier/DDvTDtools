#' Compute likelihood landscape along K for a range of trees
#'
#' Calls \code{get_likelihood_along_K()} for a range of trees in DDvTD
#'
#' @inheritParams params_doc
#' @param K_seq numeric vector, the values of K to get the likelihood for, i.e.
#' the points on which the landscape will be drawn. Default option is every 10
#' up to 100, and every 100 up to 1000, plus increased resolution around both N
#' and the true value of K (40).
#' @param fixed_pars_values character specifying which values to use for lambda0
#' and mu0. 'true_values', to use true values of the parameters as given by
#' \code{para_to_pars()}; 'true_k', to fetch ML estimates obtained starting
#' from true K, or 'from_n', to fetch ML estimates obtained starting from N.
#' @param outputfile character, the path and filename to save results at.
#'
#' @author Théo Pannetier
#' @export

run_liklandscapes <- function(
  sim,
  optim,
  para,
  rangemc = seq(1, 1000, by = 50),
  fixed_pars_values = "true_values",
  K_seq = NULL,
  outputfile = paste0(
    "data/lik_landscapes/lik_landscape_",
    "s", sim, "_o", optim, "_", para,
    "_", fixed_pars_values, ".rds"
  )
  ){
  # Check input ----------------------------------------------------------------
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_optim(optim)
  if (optim == "CR") {
    stop("function not intended for optim = CR. Please choose either DD or TD.")
  }
  assert_para(para)
  if (!(fixed_pars_values) %in% c("true_values", "true_k", "from_n")) {
    stop(
      "Incorrect input for fixed_pars_values:
       pick either 'true_values', 'true_k', or 'from_n'"
    )
  }
  if (!is.numeric(rangemc)) {
    stop("rangemc must be a numeric")
  }
  if (!is.null(K_seq) & !is.numeric(K_seq)) {
    stop("K_seq must be numeric or NULL.")
  }
  if (!is.character(outputfile)) {
    stop("outputfile must be a character.")
  }

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
    if (is.null(K_seq)) {
      K_seq <- c(seq(10, 100, by = 10), c(seq(100, 1000, by = 100)))
      zoom_around_N <- seq(from = max(5, ntips - 10), to = ntips + 10, by = 1)
      zoom_around_K <- seq(30, 50, by = 1)
      K_seq <- sort(unique(c(K_seq, zoom_around_K, zoom_around_N)))
    }

    # Set lambda0 & mu0 values -------------------------------------------------
    if (fixed_pars_values %in% c("true_k", "from_n")) {

      ML_results <- DDvTDtools::read_optim_results(
        sim = sim,
        optim = optim,
        para = para,
        init_k = fixed_pars_values
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
    liklandscape <- DDvTDtools::get_likelihood_along_K (
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

