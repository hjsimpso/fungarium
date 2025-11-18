#' Find fungal records with specific trait data
#'
#' Find trait-relevant records (i.e., records with a particular substrate, host, or habitat association) in a data set of fungal collections/observations
#' (e.g. \href{https://mycoportal.org}{MyCoPortal} or \href{https://www.gbif.org/}{GBIF} data sets).
#'
#'
#' @param data           `dwca` object
#' @param metadata_cols  Character vector containing names of columns with trait-relevant metadata. Default (c("habitat", "occurrenceRemarks", "associatedTaxa")) is based on fields in \href{https://dwc.tdwg.org/terms/}{Darwin Core archive files} that typically contain trait-relevant metadata.
#' @param pos_string     Character string ("positive") containing a \link{regular expression} that is used to find character strings within the specified metadata columns of the input data frame that contain trait-relevant keywords or phrases.
#' @param neg_string     Character string ("negative") containing a regular expression that is used to remove records that were falsely identified, via the "positive" search string, as being trait-relevant. This argument is optional.
#' @param string_clean   Logical. If TRUE (the default), strings in metadata_cols are "cleaned" prior to trait searching. This includes converting strings to lowercase and removing any punctuation (e.g., periods, commas, question marks, etc.) or extra white space.
#' @return           Filtered input data that matches search criteria.
#' @export
#'

find_trait <- function(data, metadata_cols=c("habitat", "occurrenceRemarks", "associatedTaxa"), pos_string,  neg_string=NULL, string_clean=TRUE){
  # check args
  if (!inherits(data, "dwca")) {
    stop("'data' must be of class 'dwca'. Use `as_dwca()` first.")
  }
  
  checkmate::assertCharacter(metadata_cols)
  lapply(metadata_cols, checkmate::assert_choice, choices=colnames(data), .var.name='metadata_cols')
  checkmate::assertCharacter(pos_string)
  checkmate::assertCharacter(neg_string, null.ok = TRUE)
  checkmate::assertLogical(string_clean)
  
  #clean strings
  if (string_clean){
    raw <- data[,metadata_cols]
    raw$key <- 1:nrow(data)
    data$key <- raw$key
    for (i in 1:length(metadata_cols)){
      data[[metadata_cols[i]]] <- str_clean(data[[metadata_cols[i]]])
    }
  }
  
  #Search for records with positive keywords/phrases
  row_bool_comb <- rep(NA, nrow(data))
  for (i in 1:length(metadata_cols)){
    row_bool <- re2::re2_detect(data[[metadata_cols[i]]], pos_string)
    row_bool_comb <- (row_bool_comb&!is.na(row_bool_comb))|(row_bool&!is.na(row_bool))
  }
  data <- data[row_bool_comb,]
  remove(row_bool_comb)
  
  #Remove records that have negative keywords/phrases
  if(!is.null(neg_string)){
    row_bool_comb <- rep(NA, nrow(data))
    for (i in 1:length(metadata_cols)){
      row_bool <- re2::re2_detect(data[[metadata_cols[i]]], neg_string)
      row_bool_comb <- (row_bool_comb&!is.na(row_bool_comb))|(row_bool&!is.na(row_bool))
    }
    data <- data[!row_bool_comb,]
    remove(row_bool_comb)
  }
  
  # replace with raw strings
  if (string_clean){
    idx <- match(data$key, raw$key)
    for (i in 1:length(metadata_cols)){
      data[[metadata_cols[i]]] <- raw[[metadata_cols[i]]][idx]

    }
  }
  
  return(data)
}
