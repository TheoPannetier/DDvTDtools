check_model_tag <- function(model_tag){
  data("./data/DDvTD_tags.RData")
  if( !(model_tag %in% DDvTD_tags) ){
    stop("Incorrect model specified - see data(DDvTD_tags) for accepted model inputs.")
  }

}
