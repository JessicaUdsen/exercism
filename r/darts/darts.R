score <- function(x, y) {
  rCoord <- sqrt(x**2 + y**2)
  if(rCoord <= 1){
    return(10)
  }
  if(rCoord <= 5){
    return(5)
  }
  if(rCoord <= 10){
    return(1)
  }
  else{
    return(0)
  }
}

