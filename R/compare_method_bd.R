#' Peform a comparison of integration methods for DDD::bd_ML
#'
#' This function runs bd_ML() three times for the 50 first trees in the dataset selected by the arguments.
#' Each time a different \code{method} argument is used, among \code{"lsoda"}, \code{"ode45"}, \code{"lsodes"}.
#'
#' @param sim either "DD" or "TD", the simulation model.
#' @param para the parameters code.
#'
#' @author Théo Pannetier
#'
#' @export
#'
compare_method_bd <- function(sim, para ){

  method_vector = c("lsoda","ode45","lsodes")
  rangemc = 1:50
  optim = "TD"

  # get branching times for trees in this dataset
  brts_bundle <- get_multi_brts("data/sim/", sim, para, rangemc)

  # Set output dataframe structure
  df <- data.frame(
    mc = numeric(),
    methode = factor(levels = method_vector),
    ML = numeric(),
    lambda0 = numeric(),
    mu0 = numeric(),
    K = numeric(),
    conv = numeric(),
    runtime = numeric()
  )

  # Name outputfile
  outputfile = paste0("method-", sim, "-" , optim ,"-", para, ".RData")
  # Loop through trees in the dataset
  for(mc in rangemc){
    # get branching times for this tree
    brts <- brts_bundle[[mc]]
    # capture true parameter value as starting parameter values
    initparsopt = para_to_pars(para)[2:4]
    initparsopt[3] = length(brts) + 1

    for(i in 1:3){
      methode = method_vector[i]
      print(paste("Tree", mc, " - methode", methode))

      # Set timer to 0
      timerstart = proc.time()
      # Run bd_ML with given method
      ML_output <- DDD::bd_ML(
        brts = brts,
        initparsopt = c(initparsopt[1:2],150),
        idparsopt = 1:3,
        tdmodel = 4,
        methode = methode
      )
      # Organize bd_ML output
      results_row <- data.frame(
        mc = mc,
        methode = factor(methode, levels = method_vector),
        ML = ML_output$loglik,
        lambda0 = ML_output$lambda0,
        mu0 = ML_output$mu0,
        K = ML_output$lambda1,
        conv = ML_output$conv,
        runtime = (proc.time() - timerstart)[3] # in seconds
      )
      # Append to data frame
      df <- rbind(df, results_row, make.row.names = F)
    } # repeat with next method
    save(df, file = paste0("method_test/", outputfile))
  } # next mc

}
