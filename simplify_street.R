#' A cleanAddresses Function
#' 
#' This function takes in a character vector of character address text. The entry data MUST start with street numbers but has the option to include or exclude City, State, and Zip fields
#' The result is a vector of the same length as the input data, made up of simplified street addresses only. Any City, State, or Zip will be cut off. The result can be applied directly as a new column in a dataset 
#' @param street takes in the vector of character street names (usually a column from a dataset)
#' @param numWords the number of full words the user would like to allow to follow the street number and direction.
#' @keywords street, address
#' @export
#' @examples myData$newField <- simplify_street(street = myData$rawStreetField, numWords = 2)

simplify_street <- function(street, numWords){
  # Make a column with just the beginning of street addresses (for easier grouping)
  pat1 <- paste0("^\\d+\\s+[NSEW].{0,5}\\s*(\\w+\\s?){1,", as.character(numWords),"}")  #match for a one letter N,S,E,W before the name
  pat2 <- paste0("^(\\d+\\s+){1,2}(\\w+\\s?){1,", as.character(numWords),"}") #match for words directly after the number - only use if not expecting a directional letter
  Address_Simple <- ""  # initiate empty column
  for(i in 1:length(street)){
    if(!is.na(street[i])){
      Address_Simple[i] <- ifelse(
        !is.na(str_match(street[i], regex(pat1, ignore_case = TRUE))[1]), #Check for N,S,E,W type letter match
        (str_match(street[i], regex(pat1, ignore_case = TRUE))[1] %>% #if N,S,E,W letter was found, match pattern 1 and substitute NSEW to standard format
           gsub("\\sN\\s|\\sN\\.\\s|\\sNorth\\s", "North ", x = ., ignore.case = TRUE) %>%
           gsub("\\sS\\s|\\sS\\.\\s|\\sSouth\\s", "South ", x = ., ignore.case = TRUE) %>%
           gsub("\\sE\\s|\\sE\\.\\s|\\sEast\\s", "East ", x = ., ignore.case = TRUE) %>%
           gsub("\\sW\\s|\\sW\\.\\s|\\sWest\\s", "West ", x = ., ignore.case = TRUE)), 
        str_match(street[i], regex(pat2, ignore_case = TRUE))) #else take a full word after the number using pattern 2
    }else{"NA"} # If no address, NA simple address
  } 
  Address_Simple <- gsub("apt|unit|apartment", "", Address_Simple, ignore.case = TRUE) # remove extra words
  return(Address_Simple)
}


# Samantha Lynn Bell - June 1, 2021