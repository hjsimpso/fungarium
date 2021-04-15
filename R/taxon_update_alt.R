#' Update scientific names of fungi
#'
#' Validates or updates the scientific names of fungi, and their associated taxonomic classification, based on currently accepted scientific consensus listed in the \href{https://www.gbif.org/}{GBIF} Backbone Taxonomy database.
#'
#'
#' @param data Data.frame or data.table containing a column of canonical names (e.g. "Pleurotus", "Pleurotus ostreatus") and a column of corresponding authorships (e.g. "(Fr.) P.Kumm.", "(Jacq.) P.Kumm."). Taxa listed in the dataframe can be from any taxonomic rank from kingdom to species; however, there are caveats when updated names for ranks other than species. See Simpson & Schilling (2020).
#' @param taxon_col Character string specifying the name of the column containing canonical names. Default is "scientificName" (name used in MyCoPortal datasets).
#' @param authorship_col Character string specifying the name of the column containing authorships. Default is "scientificNameAuthorship" (name used in MyCoPortal datasets).
#' @param status_feed Logical. If TRUE, progress of taxa names being queried in the GBIF database is printed on the console.
#' @param species_only Logical. If TRUE, records not identified to the species-level are removed from the dataset prior to name updates.
#' @param force_accepted Logical. If TRUE, records that do not have authorship information will be updated as the ACCEPTED full scientific name, if one exists, regardless of whether or not all potential authorships with for the given canonical names would lead to the same ACCEPTED full scientific name.
#'
#' @return The input dataframe with the following output fields appended:
#' \item{query_full_name}{exact string used in GBIF query}
#' \item{new_name}{currently accepted scientific name (may be the same as the name originally listed in the input file, meaning that the orginal name is currently accepted)}
#' \item{new_author}{authorship associated with the currently accepted scientific name}
#' \item{new_full_name}{name and authorship combined into one character string}
#' \item{new_kingdom}{kingdom classification based on the new scientific name}
#' \item{new_phylum}{phylum classification based on the new scientific name}
#' \item{new_class}{class classification based on the new scientific name}
#' \item{new_order}{order classification based on the new scientific name}
#' \item{new_family}{family classification based on the new scientific name}
#' \item{new_genus}{genus associated with the new scientific name}
#' \item{new_specific_epithet}{specific epithet asscociated with the new scientific name}
#' \item{taxon_conf}{confidence score (0-100) for match quality of the full scientific name. Ex: "Trametes versicolor (L.) Lloyd" in sporocarp dataset matched with "Trametes versicolor (L.) Lloyd" in GBIF database would give a confidence score of 100.}
#' \item{taxon_matchtype}{refers to match type of the canonical name. EXACT means a perfect match. Ex: "Trametes versicolor" in sporocarp dataset matched to "Trametes versicolor" in GBIF database. FUZZY means an imperfect match, likely due to spelling errors. Ex: "Trametes versacolor" in sporocarp dataset matched with "Trametes versicolor" in GBIF database.}
#' \item{error}{error code for why a name could not be validated or updated.
#' error1: name has doubtful taxonomic status (not accepted as a valid taxon and has no valid synonyms).
#' error2: name has no authorship listed and all GBIF matches are of a higher taxonomic rank.
#' error3: name has no authorship listed and all GBIF matches have doubtful taxonomic status.
#' error4: name has no authorship listed and all GBIF matches have different accepted GBIF usage keys (accepted keys correspond to accepted taxa).
#' error5: no matches returned from GBIF.
#' error6: the synonym of the matched GBIF record has doubtful taxonomic status.
#' error7: the synonym of the matched GBIF record is also listed as a synonym (may indicate an error within GBIF).
#' error8: name has authorship listed and the best GBIF match is of a higher taxonomic rank.}
#' @details Queries the GBIF database for each taxon, via \code{get_gbifid_}
#' in the taxize package (Chamberlain and Szocs 2013; Chamberlain et al. 2020), after correcting any erroneous capitalizations of specific epithets (e.g. "Pleurotus Ostreatus").
#' If a taxon name is mispelled in the input dataset, the best approximate match is selected from the GBIF match results.
#' See \code{taxon_conf} and \code{taxon_matchtype} in the "Value" section for more details about approximate matches.
#' Note that an internet connection is required to retrieve data from the GBIF database.\cr
#' \cr
#' If a queried taxon is matched to a GBIF record and that record has "accepted" taxonomic
#' status, the queried name is "validated" (i.e. the output "new_name" is the same as the queried name).
#' If the matched GBIF record is a "synonym", the "accepted" record associated with that
#' synomym is used to "update" the queried taxon (i.e. the ouput "new_name" is different from the queried name).\cr
#' \cr
#' If a queried taxon has no GBIF matches or the GBIF match has "doubtful"
#' taxonomic status, the queried taxon is not validated or updated (i.e. the output "new_name" will be blank) and an error code is output. See \code{error} in "Value" section.\cr
#' \cr
#' Function is tailored for large datasets of diverse taxa, like the archived sporocarp collection/observation records accessible through the \href{https://mycoportal.org/}{MyCoPortal}.
#' However, function will work on any dataset with taxon names and authorship. Becasue processing time can be significant, it may be helpful to breakup extremely large datasets (>50,000 records) and apply this function to each smaller subset.
#' This ensures that if an error (e.g. loss of internet connection) would occur while the function is running the time lost would be minimized.\cr
#' \cr
#' Subspecies in input dataset are automatically updated to the species level. The accepted synonyms of subspecies listed in the GBIF database are the species names.
#' @author Hunter J. Simpson
#' @references \enumerate{
#' \item Scott Chamberlain and Eduard Szocs (2013). taxize - taxonomic search and retrieval in R. F1000Research, 2:191. URL:http://f1000research.com/articles/2-191/v2.
#' \item Scott Chamberlain, Eduard Szoecs, Zachary Foster, Zebulun Arendsee, Carl
#' Boettiger, Karthik Ram, Ignasi Bartomeus, John Baumgartner, James O'Donnell,
#' Jari Oksanen, Bastian Greshake Tzovaras, Philippe Marchand, Vinh Tran, Maëlle
#' Salmon, Gaopeng Li, and Matthias Grenié. (2020) taxize: Taxonomic information
#' from around the web. R package version 0.9.95. https://github.com/ropensci/taxize
#' \item Simpson, H.J., Schilling, J.S. 2021. Using aggregated field collections data
#' and the novel R package fungarium to investigate fungal fire association. \emph{Mycologia}. \bold{IN PRESS}
#' }
#' @export
#' @examples
#' MP_data <- mycoportal_tab("Pleurotus")
#' MP_data_updated <- taxon_update(MP_data)
#'
taxon_update_alt <- function(data, taxon_col="scientificName", authorship_col="scientificNameAuthorship", status_feed=TRUE, species_only=TRUE, force_accepted=FALSE){
  #check that the input is formatted correctly. If not, stop and print error.
  if (!is.data.frame(data)){
    stop('Input data needs to be a data.frame.')
  }
  #Remove all non species-level taxa if species_only is TRUE; also removes any records with blanks in taxon name column
  if (species_only==TRUE){data <- data[grep("(?i)[a-z]+\\s[a-z]+",data[[taxon_col]]),]}
  data <- data[data$scientificName!="",]
  #Create new data frame with only name and authorship info
  data_cond <- data.frame(query_name=data[[taxon_col]], query_authorship=data[[authorship_col]])
  #Fix erroneous capitalization of specific epithet
  data_cond$query_name <- tolower(data_cond$query_name)
  data_cond$query_name <- gsub("([a-z])(.*)", "\\U\\1\\L\\2", data_cond$query_name, perl=T)
  #Create new table with list of unique species
  data_cond$query_full_name <- ifelse(data_cond$query_authorship!="", paste(data_cond$query_name, data_cond$query_authorship, sep = " "), data_cond$query_name)
  unique_taxa <- dplyr::distinct(data_cond)

#Name update using GBIF via taxize
  #Add columns for name update output
  out <- matrix(nrow=nrow(unique_taxa),ncol = 13)
  unique_taxa <- cbind(unique_taxa,out)
  unique_taxa[is.na(unique_taxa)] <- ""

  #update unique taxon names
  gbif_out_l <- taxize::get_gbifid_(sci = unique_taxa$query_full_name, method = "backbone", messages=status_feed)
  unique_taxa <- t(apply(unique_taxa,1, name_update_loop,status_feed=status_feed, gbif_out_l))
  unique_taxa <- data.frame(iconv(unique_taxa,from="UTF-8", to="latin1"))
  colnames(unique_taxa)[1:13] <- c("query_full_name", "new_full_name", "new_name",
                                   "new_author", "new_kingdom",
                                   "new_phylum", "new_class",
                                   "new_order", "new_family",
                                   "new_genus", "taxon_conf",
                                   "taxon_matchtype", "error")


#Put new names into original input file
  data_cond <- dplyr::inner_join(data_cond, unique_taxa, by="query_full_name")
  data_cond$new_specific_epithet <- gsub("^\\S+\\s|^\\S+$", "", data_cond$new_name)
  data <- cbind(data, data_cond[,3:16])#Remove query_name and query_authorship; keep "query_full_name" column, contains the exact query submitted to GBIF
  return(data)
}
