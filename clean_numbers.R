#' Clean phone numbers
#'
#' This function allows you to send a column/vector of dirty phone numbers through and supplies you with a list of standardized, clean phone numbers of the same vector length. Non-passing numbers will be set to NA. 
#' @param phone [The column (dat$col) or vector of phone numbers]
#' @param country [The country code that can appear at the beginning of a phone number]
#' @keywords [phone number, phone, digit]
#' @export
#' @examples
#' myDat$newPhoneNumbers <- clean_numbers(myDat$oldPhoneNumbers)



clean_numbers <- function(phones, country){
  # get rid of anything that is not a digit, and return as numbers
  phones <- as.numeric(gsub("\\D", "", phones))
  pass <- ""
  for(i in 1:length(phones)){
    
    # Find unique numbers within the phone number
    findNum <- gregexpr("[0-9]", phones[i])
    splitNum <- as.numeric(unique(unlist(regmatches(phones[i], findNum))))
    lenUnique <-length(splitNum)
    
    # Judge whether a number passes or fails
    if( 
      ( ( (nchar(phones[i])==10) ) | # has at least 10 digits
        ( (nchar(phones[i])==11) & (substring(phones[i], 1, 1)==country) ) ) & # Or has a leading 1 and is 11 digits 
      (!is.na(phones[i]) | is.null(phones[i]) | grepl("unreadable", phones[i], ignore.case=TRUE)) & # not missing
      grepl("[[:digit:]]", phones[i]) & # must consist of at least some digits  
      !(grepl("([[:digit:]])\\1{9,11}", phones[i])) & # must not be repeating one number
      (lenUnique > 3) & # Has more than 3 unique digits (not repeating numbers)
      !grepl("(123456789)", phones[i]) # not just numbers in order (fake number)
    ){
      pass[i] <- phones[i] # If passed, save the cleaned number
    }else(pass[i] <- "NA") # If failed, save as NA
  }
  # clip leading country codes if present (save 10 digits)
  pass <- substr(pass, nchar(pass)-9, nchar(pass))
  
  # Return the cleaned numbers
  return(pass)
}


