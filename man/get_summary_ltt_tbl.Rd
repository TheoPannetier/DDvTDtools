% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_summary_ltt_tbl.R
\name{get_summary_ltt_tbl}
\alias{get_summary_ltt_tbl}
\title{Get lineage-through-time summaries for a set of phylogenetic trees}
\usage{
get_summary_ltt_tbl(phylos, time_seq)
}
\arguments{
\item{phylos}{a list of phylogenetic trees.}

\item{time_seq}{a numeric vector containing the sequence of time points at
which the summaries are to be computed.}
}
\description{
Computes the mean, SD, median, and some quantiles for the number of lineages
through time across a set of trees.
}
\author{
Théo Pannetier
}
