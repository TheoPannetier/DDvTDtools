#' Check optimisation results for completeness
#' 
#' The function reads results files output from run_ML calls and prints whether some
#' results are missing or some optimisations have not converged
#' 
#' @param folder the folder where the results files are stored. All .rds files in the folder are read.
#' 
#' @author Théo Pannetier
#' 
#' @export

check_convergence_optim <- function(folder = "data/optim/"){
  assert_DDvTD_wd()
  
  files_list <-  list.files(folder, pattern = ".rds")
  
  for(file in files_list){
    res <- readRDS(paste0(folder, file))
    
    missing_rows <- which(!(1:1000 %in% res$mc))
    if(length(missing_rows) > 0){
      cat(paste("File", file, ":", length(missing_rows), "missing rows\n"))
    }
    
    notConverged <- which(res$hasConverged == FALSE)
    if(length(notConverged > 0)){
      cat(paste("File", file, ": optimisation has not converged for", length(notConverged), "trees\n"))
    }
  }
}
