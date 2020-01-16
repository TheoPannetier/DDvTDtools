#' Documentation for main parameters of the model
#'
#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#'
#' @param sim character, name of simulation model, either \code{"DD"}
#' or \code{"TD"}.
#' @param para numeric, a four-digits code specifying a set of four parameter
#' values. Refer to \code{arg_para()} doc for details, and call
#' \code{arg_para()} to see possible inputs.
#' @param optim character, name of the optimisation model, either \code{"DD"}
#' or \code{"TD"}.
#' @param init_k character code specifying the initial values of parameter K
#' used in the optimisation. Refer to \code{get_possible init_k()} doc for
#' details, and call \code{arg_init_k()} to see possible inputs.
#' @param mc numeric between 1 and 1000, the index of a tree within a dataset.
#' @param rangemc numeric vector, a set of tree indices ranging from 1 to 1000.
#' @param brts numeric vector, a set of branching times as given by
#' \code{ape::branching_times()}.
#' @param with_extinct logical, should the tree include extinct lineages?
#
#'
#' @author Theo Pannetier, based on skeleton stolen from Richel J.C. Bilderbeek.

params_doc <- function(
sim, para, optim, init_k, mc, rangemc, brts, with_extinct
) {
  # Nothing
}
