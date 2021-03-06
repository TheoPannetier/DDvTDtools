% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/params_doc.R
\name{params_doc}
\alias{params_doc}
\title{Documentation for main parameters of the model}
\usage{
params_doc(sim, para, optim, init_k, mc, rangemc, brts, with_extinct)
}
\arguments{
\item{sim}{character, name of the simulation model, either \code{"DD"}
or \code{"TD"}.}

\item{para}{numeric, a four-digits number coding for a set of four parameter
values. Refer to \code{arg_para()} doc for details, and call
\code{arg_para()} to see possible inputs.}

\item{optim}{character, name of the optimisation model, either \code{"DD"}
or \code{"TD"}.}

\item{init_k}{character code specifying the initial values of parameter K
used in the optimisation. Refer to \code{arg_init_k()} doc for
details, and call \code{arg_init_k()} to see possible inputs.}

\item{mc}{numeric between 1 and 1000, the index of a tree within a dataset.}

\item{rangemc}{numeric vector, a set of tree indices ranging from 1 to 1000.}

\item{brts}{numeric vector, a set of branching times as given by
\code{ape::branching_times()}.}

\item{with_extinct}{logical, should the tree include extinct lineages?}
}
\description{
This function does nothing. It is intended to inherit is parameters'
documentation.
}
\author{
Theo Pannetier, based on skeleton stolen from Richel J.C. Bilderbeek.
}
