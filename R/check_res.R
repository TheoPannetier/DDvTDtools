check_res <- function(sim, para, optim, init, path = "./data/optim/", return_out = F){

  # Read res data
  filename <- paste0("sim", sim, "_optim", optim, "_init", init, "-", para, ".rds")
  print(filename)
  res <- readRDS(paste0(path, filename))

  # Check metadata is correct
  check_metadata(res = res,
             sim = sim,
             para = para,
             optim = optim,
             init = init
             )

  # Check for duplicates
  duplicated_rows <- check_duplicated_rows(res = res, return_out = return_out)

  # Check for missing tree indexes
  missing_mcs <- check_missing_mcs(res = res, return_out = return_out)

  # Check for missing data within rows
  incomplete_rows <- check_incomplete_rows(res = res, return_out = return_out)

  # Check res columns type
  check_res_type(res = res)

  # Check there is a likelihood estimate for each row
  missing_loglikelihoods <- check_loglikelihood_exists(res = res, return_out = return_out)

  if(return_out){
    return(list(
      "duplicated_rows" = duplicated_rows,
      "missing_mcs" = missing_mcs,
      "incomplete_rows" = incomplete_rows,
      "missing_loglikelihoods" =  missing_loglikelihoods
    ))
  }
}

