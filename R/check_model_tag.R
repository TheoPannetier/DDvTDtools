check_model_tag <- function(model_tag){
  data("./data/DDVTD_tags.RData")
  if( !(model %in% DDvTD_tags) ){
    stop("Incorrect model specified - see data(DDvTD_tags) for accepted model inputs.")
  }

}
