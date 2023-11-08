hamming <- function(strand1, strand2) {
  if(nchar(strand1) != nchar(strand2)){
    stop('ERROR: DNA strands must be the same length!')
  }
  else{
    strandLength <- nchar(strand1)
    hammingDistance <- 0
    
    for(i in 1:strandLength){
      if(substr(strand1, i, i) != substr(strand2, i, i)){
        hammingDistance <- hammingDistance + 1
      }
    }
    
    return(hammingDistance)
  }
}
