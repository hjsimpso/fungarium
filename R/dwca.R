#' Create a DWCA object
#'
#' Validates and assigns the S3 class 'dwca' to a data.frame.
#'
#' @param data A data.frame with required Darwin Core fields. ("scientificName",
#' "scientificNameAuthorship", "eventDate", "country", "stateProvince",
#' "decimalLatitude", "decimalLongitude", "habitat", "occurrenceRemarks",
#' "associatedTaxa")
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


  # ensure all cols are character
  data <- as.data.frame(sapply(data, as.character))


  # add NAs to null strings
  data[data==""] <- NA

  # return dwca class object
  class(data) <- c("dwca", "data.frame")
  data
}

#' Import a DWCA file
#'
#' Validates and assigns the S3 class 'dwca' to a data.frame.
#'
#' @param data A data.frame with required Darwin Core fields. ("scientificName",
#' "scientificNameAuthorship", "eventDate", "country", "stateProvince",
#' "decimalLatitude", "decimalLongitude", "habitat", "occurrenceRemarks",
#' "associatedTaxa")
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

  # return dwca class object
  class(data) <- c("dwca", "data.frame")
  data
}
