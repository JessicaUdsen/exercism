word_count <- function(input){
  
  #Removes single quotes that aren't in contractions
  removeSingleQuote <- function(input){
    input <- gsub("^'|'$", "", input)
    return(input)
  }

  #String cleaning, including removing punctuation and extra spaces, etc
  cleanString <- gsub("[^a-zA-Z0-9']|\\s{2,}", " ", input)
  cleanString <- gsub("^\\s|\\s$", "", cleanString)
  cleanString <- tolower(cleanString)

  #Split string into a vector, remove elements that are empty strings, remove
  #non-contraction 's
  splitString <- unlist(strsplit(cleanString, " "))
  splitCleanString <- sapply(splitString, removeSingleQuote, USE.NAMES = FALSE)
  splitCleanString <- splitCleanString[nzchar(splitCleanString)]

return(as.list(table(splitCleanString)))
}