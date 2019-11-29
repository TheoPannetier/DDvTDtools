#' Simulate a set of phylogenetic trees under a time-dependent or
#' diversity-dependent model of diversification.
#'
#' Reads a model name and coded parameter input to simulate a dataset of a
#' number of simulated phylogenetic trees.
#'
#' @param sim a character used to select the model, can be either 'DD' or
#' 'TD'.
#' @param para a 4-digit code used to set the parameter values used for the
#' simulation. See \code{parameter_list} for possible values.
#' @param custom_pars a 4-element numeric vector containing c(age, pars)
#' to supply custom parameters (i.e., values not covered with the \code{para}
#' option). Either \code{para} or \code{custom_pars} must be supplied, not both.
#' @param nbmc Integer. How many trees should be generated?
#' @param outputfile path and name for output file, by default generated
#' automatically from \code{sim} and \code{para}.
#' @param save_results logical. Should save the results to \code{outpufile}
#' (default) or not.
#' @param return_results logical. Should results be returned? Default to
#' \code{TRUE}.
#'
#' @return A \code{list} containing itself \code{nbmc} lists corresponding a
#' simulated trees. See \code{DDD:dd_sim} for the content of these lists.
#'
#' @author Cesar Martinez and Theo Pannetier
#'
#' @export

# simulate_trees(
#   sim = "TD",
#   custom_pars = td_ml_pars,
#   outputfile = "data/setophaga/02_sim/setophaga_simTD_trees.RData",
#   save_results = TRUE,
#   return_results = FALSE
# )

simulate_trees <- function(sim,
                           para = NULL,
                           custom_pars = NULL,
                           nbmc = 1000,
                           outputfile = paste0(
                             "./data/sim/sim", sim, "-", para, ".RData"
                             ),
                           save_results = TRUE,
                           return_results = TRUE
){
  set.seed(42)
  assert_DDvTD_wd()
  assert_sim(sim)
  if (!is.numeric(nbmc)) {
    stop("argument nbmc should be a numeric.")
  }
  if (!is.character(outputfile)) {
    stop("argument outputfile should be a character.")
  }
  if (!is.logical(save_results)) {
    stop("argument save_results should be a logical")
  }
  if (!is.logical(return_results)) {
    stop("argument return_results should be a logical")
  }

  # Check model parameters input and set initial parameters
  if (is.null(para)) {
    if (is.null(custom_pars)) {
      stop("Either para or custom_pars must be supplied.")
    } else {
      if (!is.numeric(custom_pars) | length(custom_pars) != 4) {
        stop("custom_pars must be a length 4 numeric vector.")
      } else {
        age <- custom_pars[1]
        pars <- custom_pars[2:4]
      }
    }
  } else {
    if (is.null(custom_pars)) {
      assert_para(para)
      age <- DDvTDtools::para_to_pars(para)[1]
      pars <- DDvTDtools::para_to_pars(para)[2:4]
    } else {
      stop("Either para or custom_pars must be supplied, not both.")
    }
  }

  cat("Input values:", pars, "\n")
  cat(paste("File will be saved at: ", outputfile, "\n"))

  trees <- list()
  for (mc in 1:nbmc) {
    cat("Simulating tree ", mc, "...\n")
    if (sim == "DD") {
      trees[[mc]] = DDD::dd_sim(pars = pars, age = age)
    } else {
      trees[[mc]] = DDD::td_sim(pars = pars, age = age)
    }
    if (save_results) {
      save(trees, sim, pars, file = outputfile)
    }
  }
  if (return_results == TRUE) {
    return(trees)
  }
}
