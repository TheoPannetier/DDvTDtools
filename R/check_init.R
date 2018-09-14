check_init <- function(init){
  if ( !( init %in% c(1:3) ) ){ 
    stop("Init can only take values {1, 2, 3}.") 
    }
}
