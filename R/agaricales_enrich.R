#' Sample data set of MyCoPortal records
#'
#' Sample data set of fungal records downloaded from the MyCoPortal (https://mycoportal.org) on Dec 9, 2021.
#' Downloaded as Darwin core tab-delimited file in UTF-8 encoding.
#' Includes Agaricales records from the United States.
#' Taxon names updated using \code{fungarium::taxon_update}.
#' Fire-associated enrichment calculated using \code{fungarium::find_trait} and \code{fungarium::enrichment}
#'
#' @format A data frame with:
#' \describe{
#'   \item{new_full_name}{name of enrichment variable}
#'   \item{freq}{Numeric. Number of records in the full dataset}
#'   \item{trait_freq}{Numeric. Number of records in the trait dataset}
#'   \item{trait_ratio}{Numeric. trait_freq/freq}
#'   \item{coll_blanks}{Numeric. Number of total records with blank collector info.}
#'   \item{blanks_bias}{Numeric. Proportion of total records that have blank collector info.}
#'   \item{coll_blanks_t}{Numeric. Number of trait records with blank collector info.}
#'   \item{blanks_bias_t}{Numeric. Proportion of trait records that have blank collector info.}
#'   \item{max_bias}{Numeric. Max proportion of total records associated with one collector group.}
#'   \item{coll_groups}{Numeric. Number of unique collector groups for total records.}
#'   \item{max_bias_t}{Numeric. Max proportion of trait records associated with one collector group.}
#'   \item{coll_groups_t}{Numeric. Number of unique collector groups for trait records.}
#'   \item{new_kingdom}{kingdom classification based on the new scientific name}
#'   \item{new_phylum}{phylum classification based on the new scientific name}
#'   \item{new_class}{class classification based on the new scientific name}
#'   \item{new_order}{order classification based on the new scientific name}
#'   \item{new_family}{family classification based on the new scientific name}
#'   \item{new_genus}{genus associated with the new scientific name}
#'   \item{new_species}{canonical species name based on the new scientific name. This is useful for getting the species name for varieties or sub-species, which will have the full variety or subspecies names listed for "new_full_name".}
#'  }
#' @source \url{https://mycoportal.org}
"agaricales_enrich"
