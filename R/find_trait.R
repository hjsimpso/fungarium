#' Find fungal records with specific trait data
#'
#' Find records within a dataset of fungal collections/observations
#' (e.g. \href{https://mycoportal.org}{MyCoPortal} dataset)
#' that have environmental metadata relevant to a particular trait.
#' Example traits: growing on wood, soil, or leaves;
#' growing on downed log or standing tree;
#' growing on or near a specific plant taxon;
#' growing on or near other fungal taxa;
#' growing in a habitat affected by a particular disturbance (e.g. wind, fire, logging)
#'
#'
#' @param data           Data.frame containing columns of trait-relevant metadata.
#' @param pos_string     Character string ("positive") containing a \link{regular expression} that is used to find character strings within the specified metadata columns of the input data frame that contain trait-relevant keywords or phrases.
#' @param neg_string     Character string ("negative") containing a regular expression that is used to remove records that were falsely identfied, via the "positive" search string, as being trait-relevant. This argument is optional.
#' @param metadata_cols  Character vector containing names of columns with trait-relevant metadata. Default names are "occurrenceRemarks", "host", "habitat", and "substrate" (names used in MyCoPortal datasets).
#' @param string_clean   Logical. If TRUE (the default), strings in metadata_cols are "cleaned" prior to trait searching. This includes converting strings to lowercase and removing any puncuation (e.g., periods, commas, question marks, etc.) or extra white space.
#' @return           Returns a data.frame of records with trait-relevant metadata.
#' @export
#'
#' @examples
#' library(fungarium)
#' data(strophariaceae) #import sample dataset
#' data <- strophariaceae
#'
#' #Finds fire-associated records
#' string1 <- "(?i)charred|burn(t|ed)|scorched|fire.?(killed|damaged|scarred)|killed.by.fire"
#'
#' #Removes records falsely identfied as fire-associated
#' string2 <- "(?i)un.?burn(t|ed)"
#'
#' #filter out records that do not contain any environmental metadata (optional)
#' data <- data[data$occurrenceRemarks!=""|data$host!=""|
#'                    data$habitat!=""|data$substrate!="",]
#'
#' #find trait-relevant records
#' trait_data <- find_trait(data, pos_string=string1, neg_string=string2)

find_trait <- function(data, pos_string, metadata_cols=c("host", "substrate", "habitat", "occurrenceRemarks"), neg_string=NULL, string_clean=TRUE){
  #Check that the input is formatted correctly. If not, print error.
  if (!is.data.frame(data)){
    stop('Input data needs to be a data.frame.')
  }

  #clean strings
  if (string_clean){
    for (i in 1:length(metadata_cols)){
      data[[metadata_cols[i]]] <- str_clean(data[[metadata_cols[i]]])
    }
  }

  #Search for records with positive keywords/phrases
  for (i in 1:length(metadata_cols)){
    string <- paste(ifelse(exists("string", inherits = FALSE),paste(string,"| ", sep=" "), ""), "data[[metadata_cols[",i,"]]] ", "%in% ", "grep(pos_string,data[[metadata_cols[",i,"]]], value=TRUE)", sep="")
  }
  data <- data[eval(parse(text=string)),]
  remove(string)

  #Remove records that have negative keywords/phrases
  if(!is.null(neg_string)){
    for (i in 1:length(metadata_cols)){
      string <- paste(ifelse(exists("string", inherits = FALSE),paste(string,"| ", sep=" "), ""), "data[[metadata_cols[",i,"]]] ", "%in% ", "grep(neg_string,data[[metadata_cols[",i,"]]], value=TRUE)", sep="")
    }
    data <- data[!eval(parse(text=string)),]
  }
  return(data)
}

