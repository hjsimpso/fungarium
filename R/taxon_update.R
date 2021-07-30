#' Update scientific names of fungi
#'
#' Validates or updates the scientific names of fungi, and their associated taxonomic
#' classification, based on currently accepted scientific consensus listed in the
#' \href{https://www.gbif.org}{GBIF} Backbone Taxonomy database.
#'
#'
#' @param data Data.frame (must be in utf8 encoding) containing a column of canonical names (e.g. "Pleurotus", "Pleurotus ostreatus") and a column of corresponding authorships (e.g. "(Fr.) P.Kumm.", "(Jacq.) P.Kumm."). Taxa listed in the dataframe can be from any taxonomic rank from kingdom to species; however, there are caveats when updated names for ranks other than species. See Simpson & Schilling (2021).
#' @param taxon_col Character string specifying the name of the column containing canonical names. Default is "scientificName" (name used in MyCoPortal datasets).
#' @param authorship_col Character string specifying the name of the column containing authorships. Default is "scientificNameAuthorship" (name used in MyCoPortal datasets). If input dataset has no authorship column, use NULL. This assumes that no authorship information is available. Taxon names and authorship names combined in one column (e.g."Pleurotus ostreatus (Jacq.) P.Kumm." ), is currently not supported.
#' @param species_only Logical. Default if TRUE. If TRUE, records not identified to the species-level are removed from the dataset prior to name updates.
#' @param force_accepted Logical. Default is FALSE. If TRUE, records that do not have authorship information will be updated as the ACCEPTED full scientific name, if one exists, regardless of whether or not all potential authorships with for the given canonical names would lead to the same ACCEPTED full scientific name.
#' @param show_status Logical. Default is TRUE. If TRUE, percent completion and the number of unique taxa left to process is printed in the console.
#' @param show_names Logical. Default is FALSE. If TRUE, taxon names are printed on the console as they are submitted as queries to GBIF.
#' @param cores Integer. Default is 1. Specifies number of cores to use for processing. Values greater than 1 utilize parallel processing (not allowed on Windows systems). Parallel processing not recommended for use in GUI setting. See \code{parallel::mclapply}.
#'
#' @return The input data.frame with the following output fields appended:
#' \item{query_full_name}{exact string used in GBIF query}
#' \item{new_name}{currently accepted canonical name (may be the same as the name originally listed in the input file, meaning that the orginal name is currently accepted)}
#' \item{new_author}{authorship associated with the currently accepted scientific name}
#' \item{new_full_name}{name and authorship combined into one character string. Represents the full scientific name.}
#' \item{new_kingdom}{kingdom classification based on the new scientific name}
#' \item{new_phylum}{phylum classification based on the new scientific name}
#' \item{new_class}{class classification based on the new scientific name}
#' \item{new_order}{order classification based on the new scientific name}
#' \item{new_family}{family classification based on the new scientific name}
#' \item{new_genus}{genus associated with the new scientific name}
#' \item{new_specific_epithet}{specific epithet asscociated with the new scientific name}
#' \item{rank}{taxonomic rank}
#' \item{new_species}{canonical species name based on the new scientific name. This is useful for getting the species name for varieties or sub-species, which will have the full variety or subspecies names listed for "new_full_name".}
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
#' This ensures that if an error (e.g. loss of internet connection) would occur while the function is running the time lost would be minimized.
#' @author Hunter J. Simpson
#' @note Http errors may indicate issues with the GBIF database (e.g., the taxonomy backbone is being updated). Monitor GBIF system health at https://www.gbif.org.\cr
#' \cr
#' Sporadic GBIF connection errors may also occur during parallel processing. The cause of this is currently unknown,
#' but doesn't appear to be connected to GBIF system health. If an error does occur when processing a taxon name, that taxon is automatically reprocessed until an error no longer occurs.
#' So far, this solution seems to work well; however, if the error is related to something that can't immediately be fixed (e.g., GBIF system health issues),
#' the code may loop indefinitely. Track function progress output if you believe you may be experiencing this issue.
#' Progress can be tracked in different ways using either \code{show_status} or \code{show_names}.
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
#' library(fungarium)
#' data(strophariaceae) #import sample data set
#' data <- taxon_update(strophariaceae, show_status=FALSE) #update taxon names

taxon_update <- function(data, taxon_col="scientificName",
                         authorship_col="scientificNameAuthorship",
                         show_names=FALSE, species_only=TRUE,
                         force_accepted=FALSE, show_status=TRUE,
                         cores=1){
  #check that the input is formatted correctly. If not, stop and print error.
  if (!is.data.frame(data)){
    stop('Input data needs to be a data.frame.')
  }

  #if there is no authorship column in input data, create a blank authorship column; authorship column (even if blank) needed for downstream processing
  if (is.null(authorship_col)){
    data$authorship <- ""
    authorship_col <- "authorship"
  }

  #Remove all non species-level taxa if species_only is TRUE; also removes any records with blanks in taxon name column
  if (species_only){data <- data[grep("(?i)[a-z]+\\s[a-z]+",data[[taxon_col]]),]}
  #Create new data frame with only name and authorship info
  data_cond <- data.frame(query_name=data[[taxon_col]], query_authorship=data[[authorship_col]])
  #Fix erroneous capitalization of specific epithet
  data_cond$query_name <- tolower(data_cond$query_name)
  data_cond$query_name <- gsub("([a-z])(.*)", "\\U\\1\\L\\2", data_cond$query_name, perl=T)
  #Create new table with list of unique species
  for(i in 1:nrow(data_cond)){
    data_cond$query_full_name[i] <- ifelse(data_cond$query_authorship[i]!="", paste(data_cond$query_name[i], data_cond$query_authorship[i], sep = " "), data_cond$query_name[i])
  }
  unique_taxa <- dplyr::distinct(data_cond)

  #Name update using GBIF via taxize
  #Add columns for name update output
  out <- matrix(nrow=nrow(unique_taxa),ncol = 16)
  unique_taxa <- cbind(unique_taxa,out)
  unique_taxa[is.na(unique_taxa)] <- ""

  #update unique taxon names
  unique_taxa <- as.list(as.data.frame(t(unique_taxa)))
  unique_taxa <- maxjobs.mclapply2(unique_taxa, name_update_loop,cores=cores,
                                   show_names=show_names,show_status=show_status,
                                   force_accepted=force_accepted)
  unique_taxa <- data.frame(t(as.data.frame(unique_taxa)))
  #unique_taxa <- data.frame(t(as.data.frame(iconv(unique_taxa,from="UTF-8", to="latin1"))))
  colnames(unique_taxa)[1:16] <- c("query_full_name", "new_full_name", "new_name",
                                   "new_author", "new_kingdom",
                                   "new_phylum", "new_class",
                                   "new_order", "new_family",
                                   "new_genus", "new_specific_epithet",
                                   "new_species", "rank",
                                   "taxon_conf", "taxon_matchtype",
                                   "error")


  #Put new names into original input file
  data_cond <- dplyr::inner_join(data_cond, unique_taxa, by="query_full_name")
  data_cond$new_specific_epithet <- gsub("^\\S+\\s|^\\S+$", "", data_cond$new_name)
  data_cond <- data_cond[,!colnames(data_cond) %in% c("query_name", "query_authorship")]#Remove query_name and query_authorship; keep "query_full_name" column, contains the exact query submitted to GBIF
  data_cond <- sapply(data_cond, function(x){
   if("UTF-8"%in%Encoding(x)){iconv(x, from="UTF-8", to="latin1//TRANSLIT")}else{x}
  })
  data <- cbind(data, data_cond)
  #data[,(ncol(data)-15):ncol(data)] <- sapply(data[,(ncol(data)-15):ncol(data)], function(x){
   # if("UTF-8"%in%Encoding(x)){iconv(x, from="UTF-8", to="latin1")}else{x}
  #})
  #data[,(ncol(data)-14):ncol(data)] <- data.frame(iconv(as.matrix(data[,(ncol(data)-14):ncol(data)]),from="UTF-8", to="latin1"))
  if (species_only){
    data <- data[data$error!=""|data$rank=="species",]#post-update species removal; pre-update species removal not always 100% effective, but still useful to remove species before name updating - saves processing time
  }
  return(data)
}
