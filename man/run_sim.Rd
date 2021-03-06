% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_sim.R
\name{run_sim}
\alias{run_sim}
\title{Simulate a set of phylogenetic trees under a time-dependent or
diversity-dependent model of diversification.}
\usage{
run_sim(
  sim,
  para,
  nb_trees = 1000,
  seed = 42,
  outputfile = paste0("./data/sim/sim", sim, "-", para, ".RData"),
  save_results = TRUE,
  return_results = FALSE
)
}
\arguments{
\item{sim}{character, name of the simulation model, either \code{"DD"}
or \code{"TD"}.}

\item{para}{numeric, a four-digits number coding for a set of four parameter
values. Refer to \code{arg_para()} doc for details, and call
\code{arg_para()} to see possible inputs.}

\item{nb_trees}{numeric. How many trees should be generated?}

\item{seed}{numeric, the number to seed the random number generator.}

\item{outputfile}{path and name for output file, by default generated
automatically from \code{sim} and \code{para}.}

\item{save_results}{logical. Should save the results to \code{outpufile}
(default) or not.}

\item{return_results}{logical. Should results be returned? Default to
\code{TRUE}.}
}
\value{
A \code{list} containing \code{nb_trees} simulated trees. Each
element is itself a list of four elements, as the the output of
\code{\link[DDD:dd_sim]{DDD::dd_sim()}}. Refer to the relevant documentation in \code{DDD} for more
details.
}
\description{
Sets parameter values and calls \code{\link[DDD:dd_sim]{DDD::dd_sim()}} or \code{\link[DDD:td_sim]{DDD::td_sim()}}
to simulate a set of phylogenetic trees.
}
\author{
Cesar Martinez and Theo Pannetier
}
