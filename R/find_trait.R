#' Find fungal records with specific trait data
#'
#' Find trait-relevant records (i.e., records with a particular substrate, host, or habitat association) in a data set of fungal collections/observations
#' (e.g. \href{https://mycoportal.org}{MyCoPortal} or \href{https://www.gbif.org/}{GBIF} data sets).
#'
#'
#' @param data           Data.frame containing columns of trait-relevant metadata. (e.g. Darwin Core archive file)
#' @param metadata_cols  Character vector containing names of columns with trait-relevant metadata. Default (c("habitat", "occurrenceRemarks", "associatedTaxa")) is based on fields in \href{https://dwc.tdwg.org/terms/}{Darwin Core archive files} that typically contain trait-relevant metadata.
#' @param pos_string     Character string ("positive") containing a \link{regular expression} that is used to find character strings within the specified metadata columns of the input data frame that contain trait-relevant keywords or phrases.
#' @param neg_string     Character string ("negative") containing a regular expression that is used to remove records that were falsely identified, via the "positive" search string, as being trait-relevant. This argument is optional.
#' @param string_clean   Logical. If TRUE (the default), strings in metadata_cols are "cleaned" prior to trait searching. This includes converting strings to lowercase and removing any punctuation (e.g., periods, commas, question marks, etc.) or extra white space.
#' @return           Returns a data.frame of records with trait-relevant metadata.
#' @note Fields containing trait-relevant metadata may vary by data set, so metadata_cols should be optimized accordingly.
#' @export
#'
#' @examples
#' library(fungarium)
#' data(agaricales_updated) #import sample data set
#'
#' #Finds fire-associated records
#' string1 <- "(?i)charred|burn(t|ed)|scorched|fire.?(killed|damaged|scarred)|killed.by.fire"
#'
#' #Removes records falsely identified as fire-associated
#' string2 <- "(?i)un.?burn(t|ed)"
#'
#' #find trait-relevant records
#' trait_rec <- find_trait(agaricales_updated, pos_string=string1, neg_string=string2)

find_trait <- function(data, metadata_cols=c("habitat", "occurrenceRemarks", "associatedTaxa"), pos_string,  neg_string=NULL, string_clean=TRUE){
  #Check that the input is formatted correctly. If not, print error.
  if (!is.data.frame(data)){
    stop('Input data needs to be a data.frame.')
  }
  #Check that all metadata cols exist in input data
  for (i in 1:length(metadata_cols)){
    if (!metadata_cols[i]%in%colnames(data)){
      stop(paste0("'",metadata_cols[i],"'", " field not found in input data. Please use valid values for metadata_cols."))
    }
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

