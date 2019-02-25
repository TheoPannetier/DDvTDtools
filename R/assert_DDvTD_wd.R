#' Assert that DDvTD is the working directory
#'
#' Most functions in DDvTDtools rely on a relative path from DDvTD folder to access data and save files.
#' This function asserts it is the case.
#'
#' @author Théo Pannetier
#'
#' @export
assert_DDvTD_wd <- function(){
  working_dir <- getwd()
  nchar_path <- nchar(working_dir)
  if(substr(working_dir, nchar_path-4, nchar_path) != "DDvTD"){
    stop("Please set working dir to root of DDvTD folder before using DDvTDtools")
  }
}
