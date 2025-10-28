#helper function: str_clean
# function to remove all white space
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

# function to clean strings (lower case, remove punctation, remove all white space)
str_clean <- function(strings, periods=" ", letter_thresh=0, convert_to_ascii=T) {
  strings <- gsub("\\.", periods, strings)
  strings <- trim(tm::stripWhitespace(tm::removePunctuation(tolower(strings))))
  if (letter_thresh>0){
    strings <- stringr::str_split(strings,pattern=" ", simplify = F)
    strings <- lapply(strings, function(x) x[nchar(x)>letter_thresh])
    strings <- sapply(strings, paste, collapse=" ")
  }

  if (convert_to_ascii){
    # Convert the string to ASCII, which removes the diacritic
    strings <- stringi::stri_trans_general(str = strings, id = "Latin-ASCII")
  }

  return(strings)
}
