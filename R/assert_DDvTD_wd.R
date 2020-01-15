#' Assert that DDvTD is the working directory
#'
#' Most functions in DDvTDtools rely on a relative path from DDvTD folder to access data and save files.
#' This function asserts it is the case.
#'
#' @author Théo Pannetier
#'
#' @export
assert_DDvTD_wd <- function() {
  if (!dir.exists("../DDvTD")) {
    stop("Please set working dir to /DDvTD before calling this function.")
  }
}
