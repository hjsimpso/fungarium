#' Sample data set of MyCoPortal records
#'
#' Sample data set of fungal records downloaded from the MyCoPortal (https://mycoportal.org) on Dec 8, 2021.
#' Includes Strophariaceae records in the United States.
#' Downloaded as Darwin core tab-delimited file in UTF-8 encoding.
#'
#' @format A data frame with:
#' \describe{
#'   \item{id}{unique MyCoPortal ID for each record}
#'   \item{institutionCode}{code for the collection that houses the specimen, or digital observation}
#'   \item{scientificName}{canonical taxon name for each record}
#'   \item{scientificNameAuthorship}{authorship name(s) associated with the canonical name}
#'   \item{recordedBy}{name of collector(s)}
#'   \item{eventDate}{exact date of collection or observation}
#'   \item{year}{year in which the specimen was collected or observed}
#'   \item{occurrenceRemarks}{environmental metadata}
#'   \item{habitat}{environmental metadata}
#'   \item{associatedTaxa}{environmental metadata}
#'   \item{country}{country where specimen was collected or observed}
#'   \item{stateProvince}{state or province where specimen was collected or observed}
#'   \item{county}{county in which the specimen was observed or collected}
#'   \item{decimalLatitude}{latitude where the specimen was observed or collected}
#'   \item{decimalLongitude}{longitude where the specimen was observed or collected}
#'   \item{references}{link to MyCoPortal online record}
#' }
#' @source \url{https://mycoportal.org}
"strophariaceae"
