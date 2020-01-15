#' Set the structure of the DDvTD directory and enter it
#'
#' Creates a "DDvTD" folder and the nested folders used by functions in
#' \code{DDvTDtools}.
#'
#' @author Theo Pannetier
#' @export

set_DDvTD_dir_struct <- function() {
  dir.create("DDvTD")
  dir.create("DDvTD/data")
  dir.create("DDvTD/data/sim")
  dir.create("DDvTD/data/optim")
  dir.create("DDvTD/figures")
  setwd("DDvTD")
}
