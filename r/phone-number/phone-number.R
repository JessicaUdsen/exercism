parse_phone_number <- function(number_string) {
  
  cleanedString <- gsub("\\D+", "", number_string)
  
  #Needs to fit this regex pattern:
  #Begins with optional 1
  #Next digit needs to be between 2 and 9
  #2 more digits that are unrestricted
  #Another digit that needs to be between 2 and 9
  #Another 6 unrestricted digits
  #END
  pattern <- "^1?[2-9]\\d{2}[2-9]\\d{6}$"
  
  if (!grepl(pattern, cleanedString)){
    warning('Warning: Invalid phone number')
    return(NULL)
  }else{
    gsub("^1", "", cleanedString)
  }
}
