#' Sample data set of MyCoPortal records
#'
#' Sample data set of fungal records downloaded from the MyCoPortal (https://mycoportal.org) on Dec 8, 2021.
#' Includes Strophariaceae records in the United States.
#' Downloaded as Darwin core tab-delimited file in UTF-8 encoding.
#' Taxon names updated using \code{fungarium::taxon_update}.
#' Records with name update error were removed.
#' Records with species now classified outside of Strophariaceae were removed.
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
#'   \item{query_full_name}{exact string used in GBIF query}
#'   \item{new_name}{currently accepted canonical name (may be the same as the name originally listed in the input file, meaning that the orginal name is currently accepted)}
#'   \item{new_author}{authorship associated with the currently accepted scientific name}
#'   \item{new_full_name}{name and authorship combined into one character string. Represents the full scientific name.}
#'   \item{new_kingdom}{kingdom classification based on the new scientific name}
#'   \item{new_phylum}{phylum classification based on the new scientific name}
#'   \item{new_class}{class classification based on the new scientific name}
#'   \item{new_order}{order classification based on the new scientific name}
#'   \item{new_family}{family classification based on the new scientific name}
#'   \item{new_genus}{genus associated with the new scientific name}
#'   \item{new_specific_epithet}{specific epithet asscociated with the new scientific name}
#'   \item{rank}{taxonomic rank}
#'   \item{new_species}{canonical species name based on the new scientific name. This is useful for getting the species name for varieties or sub-species, which will have the full variety or subspecies names listed for "new_full_name".}
#'   \item{taxon_conf}{confidence score (0-100) for match quality of the full scientific name. Ex: "Trametes versicolor (L.) Lloyd" in sporocarp dataset matched with "Trametes versicolor (L.) Lloyd" in GBIF database would give a confidence score of 100.}
#'   \item{taxon_matchtype}{refers to match type of the canonical name. EXACT means a perfect match. Ex: "Trametes versicolor" in sporocarp dataset matched to "Trametes versicolor" in GBIF database. FUZZY means an imperfect match, likely due to spelling errors. Ex: "Trametes versacolor" in sporocarp dataset matched with "Trametes versicolor" in GBIF database.}
#'   \item{error}{error code for why a name could not be validated or updated}
#'  }
#' @source \url{https://mycoportal.org}
"strophariaceae_updated"
