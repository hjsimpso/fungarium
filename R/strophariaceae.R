#' Sample dataset of MyCoPortal records
#'
#' Sample dataset of fungal records downloaded from the MycoPortal (https://mycoportal.org) on March 10, 2021.
#' Includes all Strophariaceae records in the United States.
#' Downloaded as tab-delimited file in ISO-8859-1 encoding.
#'
#' @format A data frame with 89 rows and 52567 variables:
#' \describe{
#'   \item{id}{unique MyCoPortal ID for each record}
#'   \item{institutionCode}{code for the collection that houses the specimen, or digital observation}
#'   \item{scientificName}{canonical taxon name for each record}
#'   \item{scientificNameAuthorship}{authorship name(s) associated with the canonical name}
#'   \item{recordedBy}{name of collector(s)}
#'   \item{associatedCollectors}{name of associated collector(s)}
#'   \item{eventDate}{exact date of collection or observation}
#'   \item{year}{year in which the specimen was collected or observed}
#'   \item{occurrenceRemarks}{environmental metadata}
#'   \item{habitat}{environmental metadata}
#'   \item{substrate}{environmental metadata}
#'   \item{host}{environmental metadata}
#'   \item{country}{country where specimen was collected or observed}
#'   \item{stateProvince}{state or province where specimen was collected or observed}
#'   \item{county}{county in which the specimen was observed or collected}
#'   \item{decimalLatitude}{latitude where the specimen was observed or collected}
#'   \item{decimalLongitude}{longitude where the specimen was observed or collected}
#'   \item{references}{link to MyCoPortal online record}
#' }
#' @source \url{https://mycoportal.org}
"strophariaceae"
