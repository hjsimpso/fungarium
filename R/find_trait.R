#' Find fungal records with specific trait data
#'
#' Find records within a dataset of fungal collections/observations
#' (e.g. \href{https://mycoportal.org/portal/index.php}{MyCoPortal} dataset)
#' that have environmental metadata relevant to a particular trait.
#' Example traits: growing on wood, soil, or leaves;
#' growing on downed log or standing tree;
#' growing on or near a specific plant taxon;
#' growing on or near other fungal taxa;
#' growing in a habitat affected by a particular disturbance (e.g. wind, fire, logging)
#'
#'
#' @param data           Dataframe containing columns of trait-relevant metadata.
#' @param pos_string     Character string ("positive") containing a \link{regular expression} that is used to find character strings within the specified metadata columns of the input data frame that contain trait-relevant keywords or phrases.
#' @param neg_string     Character string ("negative") containing a regular expression that is used to remove records that were falsely identfied, via the "positive" search string, as being trait-relevant. This argument is optional.
#' @param metadata_cols  Character vector containing names of columns with trait-relevant metadata. Default names are "occurrenceRemarks", "host", "habitat", and "substrate" (names used in MyCoPortal datasets).
#'
#' @return           Returns a dataframe of records with trait-relevant metadata.
#' @export
#'
#' @examples
#' MP_data <- mycoportal_tab("Pleurotus")
#' MP_data_updated <- taxon_update(MP_data)
#'
#' #Finds records that are relevant to fire association
#'   string1 <- "(?i)charred|(?i)burn(t|ed)|(?i)scorched|
#'     (?i)fire.?(killed|damaged|scarred)|(?i)killed.by.fire"
#'
#' #Removes records that were falsely identfied as
#' being fire-associated because they contain the words "burnt" or "burned"
#'   string2 <- "(?i)un.?burn(t|ed)"
#'
#' trait_records <- find_trait(MP_data_updated, string1,
#'   trait_columns, string2)

find_trait <- function(data, pos_string, metadata_cols=c("host", "substrate", "habitat", "occurrenceRemarks"), neg_string=NULL){
  #Check that the input is formatted correctly. If not, print error.
  if (!is.data.frame(data)){
    stop('Input data needs to be a data.frame.')
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

