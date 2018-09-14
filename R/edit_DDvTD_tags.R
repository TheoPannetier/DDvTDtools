#' Edit the model tags accepted by DDvTDtools
#'
#' Overwrites the vector \code{DDvTD_labels} in \code{/data}. Tags should be very short, 2-4 characters,
#' e.g. \code{"DD"} for diversity-dependent diversification or \code{"CR"} for constant-rate diversification.
#'
#' @param tags a character vector containing **all** the model tags to be accepted (does not append).
#'
#' @return None.
#'
#' @author Theo Pannetier, \email{t.s.c.pannetier@rug.nl}

edit_DDvTD_tags <- function(tags){
  DDvTD_tags <- tags
  devtools::use_data(DDvTD_tags)
}
