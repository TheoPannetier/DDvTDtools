#' Simulate a set of phylogenetic trees under a time-dependent or
#' diversity-dependent model of diversification.
#'
#' Sets parameter values and calls [DDD::dd_sim()] or [DDD::td_sim()]
#' to simulate a set of phylogenetic trees.
#'
#' @inheritParams params_doc
#' @param nb_trees numeric. How many trees should be generated?
#' @param seed numeric, the number to seed the random number generator.
#' @param outputfile path and name for output file, by default generated
#' automatically from \code{sim} and \code{para}.
#' @param save_results logical. Should save the results to \code{outpufile}
#' (default) or not.
#' @param return_results logical. Should results be returned? Default to
#' \code{TRUE}.
#'
#' @return A \code{list} containing \code{nb_trees} simulated trees. Each
#' element is itself a list of four elements, as the the output of
#' [DDD::dd_sim()]. Refer to the relevant documentation in `DDD` for more
#' details.
#'
#' @author Cesar Martinez and Theo Pannetier
#'
#' @export

run_sim <- function(sim,
                    para,
                    nb_trees = 1000,
                    seed = 42,
                    outputfile = paste0(
                      "./data/sim/sim", sim, "-", para, ".RData"
                    ),
                    save_results = TRUE,
                    return_results = FALSE
){
  set.seed(seed)
  assert_DDvTD_wd()
  assert_sim(sim)
  assert_para(para)
  if (!is.numeric(nb_trees)) {
    stop("argument nb_trees should be a numeric.")
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

  age <- DDvTDtools::para_to_pars(para)[1]
  pars <- DDvTDtools::para_to_pars(para)[2:4]

  cat("Input values:", pars, "\n")
  cat(paste("File will be saved at: ", outputfile, "\n"))

  trees <- list()
  for (mc in 1:nb_trees) {
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
