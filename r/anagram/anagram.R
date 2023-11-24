anagram <- function(subject, candidates) {
  # A word is not its own anagram: for example, `"stop"` is not an anagram of `"stop"`.
  # 
  # Given a target word and a set of candidate words, this exercise requests the anagram set: the subset of the candidates that are anagrams of the target.
  # 
  # The target and candidates are words of one or more ASCII alphabetic characters (`A`-`Z` and `a`-`z`).
  # Lowercase and uppercase characters are equivalent
  
  #This function sorts the letters in a string in alphabetical order. I got this
  #from StackExchange, see 
  #https://stackoverflow.com/questions/5904797/how-to-sort-letters-in-a-string
  if (grepl("[^a-zA-Z]", subject)){
    warning('Warning: Invalid subject')
    return(c())
  }
  
  strSort <- function(string){
    return(paste(sort(unlist(strsplit(string, ""))), collapse = ""))
  }
  
  anagrams <- c()
  lowerCaseSubject <- tolower(subject)
  sortedSubject <- strSort(lowerCaseSubject)
  
  for(candidate in candidates){
    lowerCaseCandidate <- tolower(candidate)
    if(lowerCaseCandidate != lowerCaseSubject & !grepl("[^a-zA-Z]", lowerCaseCandidate)){
      sortedCandidate <- strSort(lowerCaseCandidate)
      if(sortedCandidate == sortedSubject){
        anagrams <- c(anagrams, candidate)
      }
    }
  }
  return(anagrams)
}

