raindrops <- function(number) {

  result <- ''
  mod3 <- number %% 3
  mod5 <- number %% 5
  mod7 <- number %% 7
  
  if(mod3 != 0 && mod5 != 0 && mod7 != 0 ){
    return(as.character(number))
  }
  if(mod3 == 0){
    result <- paste0(result, 'Pling')
  }
  if(mod5 == 0){
    result <- paste0(result, 'Plang')
  }
  if(mod7 == 0){
    result <- paste0(result, 'Plong')
  }
  
  return(result)
}
