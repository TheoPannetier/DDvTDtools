#' Load a simulated trees dataset
#'
#' Loads a file containing simulated trees at the specified location.
#' Target file must follow nomeclature 'simXX-para.RData'
#'
#' @inheritParams params_doc
#'
#' @author Theo Pannetier
#' @export

read_sim <- function(sim, para) {
  assert_DDvTD_wd()
  assert_para(para)
  assert_sim(sim)

  filename = paste0("sim", sim, "-", para, ".RData")

  if (!file.exists(paste0("data/sim/",filename))) {
    stop(paste0("data/sim/", filename, " does not exist"))
  }

  load(paste0("data/sim/", filename))

  # Assert we are dealing with the right class of object
  if(!ape::is.binary.phylo(trees[[1]][[1]])) {
    stop(
      paste0("data/sim/", filename, " is not a list of binary phylo objects.")
    )
  }
  return(trees)
}
