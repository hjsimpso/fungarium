#' Create a DWCA object
#'
#' Validates and assigns the S3 class 'dwca' to a data.frame.
#'
#' @param data A data.frame with required Darwin Core fields. ("scientificName",
#' "scientificNameAuthorship", "eventDate", "country", "stateProvince",
#' "decimalLatitude", "decimalLongitude", "habitat", "occurrenceRemarks",
#' "associatedTaxa")
#' @param drop_cols Logical. Drop all nonrequired columns. Default: TRUE.
#'
#' @return An object of class 'dwca' (and 'data.frame').
#' @examples
#' data <- data.frame(scientificName = "Quercus alba", occurrenceID = "123")
#' dw <- as_dwca(data, required_cols = c("scientificName", "occurrenceID"))
#' class(dw)  # c("dwca", "data.frame")
#' @export

as_dwca <- function(data) {
  req_cols = c("scientificName", "scientificNameAuthorship", "eventDate",
               "country", "stateProvince", "decimalLatitude",
               "decimalLongitude", "habitat", "occurrenceRemarks",
               "associatedTaxa")

  # check args
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data)
  if (any(!req_cols %in% names(data))) {
    coll$push(paste0("The following required columns are not found in `data`: ",
                     paste0(req_cols[!req_cols %in% names(data)], collapse = ", ")))
  }
  checkmate::reportAssertions(coll)


  # ensure all req cols are character
  col_check <- lapply(data[,req_cols], is.character)
  if (F%in%col_check){
    data[,req_cols] <- as.data.frame(sapply(data[,req_cols], as.character))
  }

  # add NAs to null strings
  data[data==""] <- NA


  # add cleaning attributes
  attr(data, "clean_date") <- FALSE
  attr(data, "clean_geography") <- FALSE
  attr(data, "clean_taxonomy") <- FALSE


  # return dwca class object
  class(data) <- c("dwca", "data.frame")
  return(data)
}

#' Import a DWCA file
#'
#' Validates and assigns the S3 class 'dwca' to a data.frame.
#'
#' @param file A file with required Darwin Core fields. ("scientificName",
#' "scientificNameAuthorship", "eventDate", "country", "stateProvince",
#' "decimalLatitude", "decimalLongitude", "habitat", "occurrenceRemarks",
#' "associatedTaxa")
#' @param sep description
#' @param quote ...
#' @param select description
#' @param drop description
#'
#' @return An object of class 'dwca' (and 'data.frame').
#'
#' @export

import_dwca <- function(file, sep='\t', quote = "\"", select=NULL, drop=NULL) {
  req_cols = c("scientificName", "scientificNameAuthorship", "eventDate",
               "country", "stateProvince", "decimalLatitude",
               "decimalLongitude", "habitat", "occurrenceRemarks",
               "associatedTaxa")

  # check args
  checkmate::assert_character(sep)
  checkmate::assert_character(quote)
  checkmate::assert_character(select, null.ok = T)
  checkmate::assert_character(drop, null.ok = T)

  # import file
  data = data.table::fread(file=file, sep = sep, quote = quote,
                           select = c(req_cols, select), data.table = F,
                           colClasses = "character")

  # check args
  coll <- checkmate::makeAssertCollection()
  if (any(!req_cols %in% names(data))) {
    coll$push(paste0("The following required columns are not found in `data`: ",
                     paste0(req_cols[!req_cols %in% names(data)], collapse = ", ")))
  }
  checkmate::reportAssertions(coll)

  # add NAs to null strings
  data[data==""] <- NA

  # add cleaning attributes
  attr(data, "clean_date") <- FALSE
  attr(data, "clean_geography") <- FALSE
  attr(data, "clean_taxonomy") <- FALSE

  # return dwca class object
  class(data) <- c("dwca", "data.frame")
  data
}
