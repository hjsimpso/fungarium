#' Update scientific names of fungi
#'
#' Validates or updates the scientific names of fungi, and their associated taxonomic classification, based on currently accepted scientific consensus listed in the \href{https://www.gbif.org/}{GBIF} Backbone Taxonomy database.
#'
#'
#' @param data Dataframe containing a column of canonical names (e.g. "Pleurotus", "Pleurotus ostreatus") and a column of corresponding authorships (e.g. "(Fr.) P.Kumm.", "(Jacq.) P.Kumm."). Taxa listed in the dataframe can be from any taxonomic rank from kingdom to species; however, there are caveats when updated names for ranks other than species. See Simpson & Schilling (2020).
#' @param taxon_name Character string specifying the name of the column containing canonical names. Default is "scientificName" (name used in MyCoPortal datasets).
#' @param authorship Character string specifying the name of the column containing authorships. Default is "scientificNameAuthorship" (name used in MyCoPortal datasets).
#' @param status_feed Logical. If TRUE, progress of taxa names being queried in the GBIF database is printed on the console.
#' @param species_only Logical. If TRUE, records not identified to the species-level are removed from the dataset prior to name updates.
#'
#' @return The input dataframe with the following output fields appended:
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
#' \item{confidence}{confidence score (0-100) for match quality of the full scientific name. Ex: "Trametes versicolor (L.) Lloyd" in sporocarp dataset matched with "Trametes versicolor (L.) Lloyd" in GBIF database would give a confidence score of 100.}
#' \item{match_type}{refers to match type of the canonical name. EXACT means a perfect match. Ex: "Trametes versicolor" in sporocarp dataset matched to "Trametes versicolor" in GBIF database. FUZZY means an imperfect match, likely due to spelling errors. Ex: "Trametes versacolor" in sporocarp dataset matched with "Trametes versicolor" in GBIF database.}
#' \item{error}{error code for why a name could not be validated or updated. See function code for information on specific error codes.}
#' @details Queries the GBIF database for each taxon, via \code\link[taxize]{get_gbifid_}
#' in the taxize package (Chamberlain and Szocs 2013; Chamberlain et al. 2020), after correcting any erroneous capitalizations of specific epithets (e.g. "Pleurotus Ostreatus").
#' If a taxon name is mispelled in the input dataset, the best approximate match is selected from the GBIF match results.
#' See \code{confidence} and \code{match_type} in the "Value" section for more details about approximate matches.
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
#' \item Simpson, H.J., Schilling, J.S. 2020. Using aggregated field collections data
#' and the novel R package fungarium to investigate fungal fire association. \emph{Mycologia}.
#' }
#' @export
#'
#' @examples
#' MP_data <- mycoportal_tab("Pleurotus")
#' MP_data_updated <- taxon_update(MP_data)
#'
taxon_update <- function(data, taxon_name="scientificName", authorship="scientificNameAuthorship", status_feed=TRUE, species_only=TRUE){
  #check that the input is formatted right. If not, stop, throw an error.
  if (!is.data.frame(data)){
    stop('Input data needs to be a data.frame.')
  }
  #Remove all non species-level taxa if species_only is TRUE
  if (species_only==TRUE){data <- subset(data, (data[[taxon_name]] %in% grep("(?i)[a-z]+\\s[a-z]+.*",data[[taxon_name]], value = T)))}
  #Create new data frame with only name and authorship info
  data_cond <- data.frame(cbind(data[[taxon_name]], data[[authorship]]))
  colnames(data_cond) <- c("query_name", "query_authorship")
  #Fix erroneous capitalization of specific epithet
  data_cond$query_name <- tolower(data_cond$query_name)
  data_cond$query_name <- gsub("([a-z])(.*)", "\\U\\1\\L\\2", data_cond$query_name, perl = TRUE)
  #Create new table with list of unique species
  data_cond$query_full_name <- paste(data_cond$query_name, data_cond$query_authorship, sep = " ")
  unique_taxa <- dplyr::distinct(data_cond)

#Name update using GBIF via taxize
  #Add columns for name update output
  out <- data.frame(matrix(nrow=nrow(unique_taxa),ncol = 12))
  colnames(out) <- c("new_name", "new_author", "new_full_name", "kingdom",
                     "phylum", "class", "order", "family",
                     "genus", "confidence",
                     "match_type", "error")
  unique_taxa <- cbind(unique_taxa,out)
  unique_taxa[is.na(unique_taxa)] <- ""

  for (i in 1:length(unique_taxa$query_name)){
    one_row_analysis <- FALSE
    gbif_new_key <- ""
    gbif_out_df <- taxize::get_gbifid_(sci = unique_taxa$query_full_name[i], method = "backbone", messages=status_feed)[[unique_taxa$query_full_name[i]]]
    if (nrow(gbif_out_df) == 0){ #No records returned from GBIF
      unique_taxa$error[i] <- "error5"
    } else{ #One or more records returned from GBIF
      if (unique_taxa$query_authorship[i] != ""){#Author listed for input record
        gbif_out_row <- gbif_out_df[1, ]#Take the first (best) match returned from GBIF
        if (gbif_out_row$matchtype == "HIGHERRANK"){#Check that best match is not a HIGHERRANK taxon match
          unique_taxa$error[i] <- "error8"
        }else {
          one_row_analysis <- TRUE
        }
      }else {#No author listed for input record
        gbif_out_rows <- gbif_out_df[gbif_out_df$matchtype == "EXACT", ]#Check for EXACT matches
        if (nrow(gbif_out_rows) == 0){#No EXACT matches
          if (nrow(gbif_out_df[gbif_out_df$matchtype != "HIGHERRANK", ])==0){#All non-EXACT matches are HIGHERRANK-can't update name
            unique_taxa$error[i] <- "error2"
          }else{#Some non-exact matches are FUZZY, can proceed with name update
           gbif_out_rows <- gbif_out_df[gbif_out_df$matchtype != "HIGHERRANK", ]#Remove HIGHERRANK matches and keep FUZZY matches
          }
        }
        if (nrow(gbif_out_rows)==1){#one match, can be FUZZY or EXACT
          one_row_analysis <- TRUE
        }
        if (nrow(gbif_out_rows)>1) {#more than one match, can be all FUZZY or all EXACT
          for (j in 1:nrow(gbif_out_rows)){#Check if all matches lead to the same valid or updated name
            if (gbif_out_rows$status[j] == "ACCEPTED"){ #Record has ACCEPTED status
              gbif_out_rows$new_key[j] <- gbif_out_rows$usagekey[j] #New key the same as "old" key
            } else {
              if (gbif_out_rows$status[j] == "DOUBTFUL"){#Record has DOUBTFUL status
                gbif_out_rows$new_key[j] <- "" #No new key
              } else { #Record has SYNONYM status
                gbif_out_rows$new_key[j] <- gbif_out_rows$acceptedusagekey[j] #New key is the key of the SYNONYM taxon
              }
            }
          }
          if (length(unique(gbif_out_rows$new_key)) == 1) {#New key the same for each match
            if (unique(gbif_out_rows$new_key) == ""){ #All matches had DOUBTFUL status, so no new key - can't update name
              unique_taxa$error[i] <- "error3"
            }else{#Matches were either ACCEPTED or SYNONYM status and resulted in one unique new key
              if (nrow(gbif_out_rows[gbif_out_rows$status== "ACCEPTED",])>0){#When matches include ACCEPTED record
                one_row_analysis <- TRUE
                gbif_out_row <- gbif_out_rows[gbif_out_rows$status== "ACCEPTED",]
              }else{#When matches are all SYNONYM status
                gbif_new_key <- gbif_out_rows$new_key[1]
                gbif_out_row <- gbif_out_rows[1,]#Get matchtype and confidence info from best (highest conf) SYNONYM match
              }
            }
          } else {#Matches have different new keys - can't update name
            unique_taxa$error[i] <- "error4"
          }
        }
      }
    }
    if (one_row_analysis == TRUE){
      if (gbif_out_row$status == "ACCEPTED"){ #Don't need to go to "new Key" gbif page
        unique_taxa$new_full_name[i] <- gbif_out_row$scientificname
        unique_taxa$new_name[i] <- gbif_out_row$canonicalname
        unique_taxa$confidence[i] <- gbif_out_row$confidence
        unique_taxa$match_type[i] <- gbif_out_row$matchtype
        unique_taxa$new_author[i] <- gsub(paste(gbif_out_row$canonicalname, " ", sep = ""), "", gbif_out_row$scientificname)
        if (is.null(gbif_out_row$genus) == "FALSE"){unique_taxa$genus[i] <- gbif_out_row$genus}
        if (is.null(gbif_out_row$family) == "FALSE"){unique_taxa$family[i] <- gbif_out_row$family}
        if (is.null(gbif_out_row$order) == "FALSE"){unique_taxa$order[i] <- gbif_out_row$order}
        if (is.null(gbif_out_row$class) == "FALSE"){unique_taxa$class[i] <- gbif_out_row$class}
        if (is.null(gbif_out_row$phylum) == "FALSE"){unique_taxa$phylum[i] <- gbif_out_row$phylum}
        unique_taxa$kingdom[i] <- gbif_out_row$kingdom
      } else {# status is not accepted
        if (gbif_out_row$status == "DOUBTFUL"){
          unique_taxa$error[i] <- "error1"
        } else {#synonym, go to new key gbif page to get new name info
          gbif_new_key <- gbif_out_row$acceptedusagekey
        }
      }
    }
  #Following loop accounts for the fact that even though some taxa are listed as synonyms in get_gbifid_ output, the synonym itself may have DOUBTFUL status (i.e. you have to check the status of the synonym as well)
  #Additionally, gbif_name_usage is used here instead of get_gbifid_, because the original query using get_gbifid_ outputs the synonym name with no authorship. Using the acceptedusagekey (given in the original get_gbifid_ output) with gbif_name_usage you can retrieve the new name AND authorship.
    if(gbif_new_key != ""){ #Go to new key page to extract name info
      key_record <- taxize::gbif_name_usage(key = gbif_new_key)
      if (key_record$taxonomicStatus == "DOUBTFUL"){#New name has DOUBTFUL status - can't update name
        unique_taxa$error[i] <- "error6"
      } else {
        if (key_record$taxonomicStatus == "SYNONYM"){#New name is still a synonym (likely indicates an error within GBIF) - can't update name
          unique_taxa$error[i] <- "error7"
        }else{#New name is ACCEPTED
          unique_taxa$new_name[i] <- key_record$canonicalName
          unique_taxa$new_full_name[i] <- key_record$scientificName
          unique_taxa$confidence[i] <- gbif_out_row$confidence
          unique_taxa$match_type[i] <- gbif_out_row$matchtype
          unique_taxa$new_author[i] <- gsub(paste(key_record$canonicalName, " ", sep = ""), "", key_record$scientificName)
          if (is.null(key_record$genus) == "FALSE"){unique_taxa$genus[i] <- key_record$genus}
          if (is.null(key_record$family) == "FALSE"){unique_taxa$family[i] <- key_record$family}
          if (is.null(key_record$order) == "FALSE"){unique_taxa$order[i] <- key_record$order}
          if (is.null(key_record$class) == "FALSE"){unique_taxa$class[i] <- key_record$class}
          if (is.null(key_record$phylum) == "FALSE"){unique_taxa$phylum[i] <- key_record$phylum}
          unique_taxa$kingdom[i] <- key_record$kingdom
        }
      }
    }
  }
  if(exists("gbif_out_rows", inherits = FALSE)){remove(gbif_out_rows)}
  if(exists("gbif_out_row", inherits = FALSE)){remove(gbif_out_row)}

#Put new names into original input file
  #Add columns for name update output
  out <- data.frame(matrix(nrow=nrow(data_cond),ncol = 13))
  colnames(out) <- c("new_name", "new_author", "new_full_name", "new_kingdom",
                     "new_phylum", "new_class", "new_order", "new_family",
                     "new_genus", "new_specific_epithet", "confidence",
                     "match_type", "error")
  data_cond <- cbind(data_cond,out)
  data_cond[is.na(data_cond)] <- ""

  for (i in 1:length(data_cond$query_name)){
    new_name_row <- unique_taxa[unique_taxa$query_full_name == data_cond$query_full_name[i], ]
    data_cond$new_name[i] <- new_name_row$new_name
    data_cond$new_author[i] <- new_name_row$new_author
    data_cond$new_full_name[i] <- new_name_row$new_full_name
    data_cond$new_kingdom[i] <- new_name_row$kingdom
    data_cond$new_phylum[i] <- new_name_row$phylum
    data_cond$new_class[i] <- new_name_row$class
    data_cond$new_order[i] <- new_name_row$order
    data_cond$new_family[i] <- new_name_row$family
    data_cond$new_genus[i] <- new_name_row$genus
    data_cond$new_specific_epithet[i] <- gsub("\\s.*", "", gsub("^\\S*\\s|^\\S*$", "", new_name_row$new_name, perl = TRUE),
                                              perl = TRUE)
    data_cond$confidence[i] <- new_name_row$confidence
    data_cond$match_type[i] <- new_name_row$match_type
    data_cond$error[i] <- new_name_row$error
    data_cond$new_name[i] <- new_name_row$new_name
    data_cond$new_author[i] <- new_name_row$new_author
    data_cond$new_full_name[i] <- new_name_row$new_full_name
  }

  data <- cbind(data, data_cond[,!colnames(data_cond) %in% c("query_name", "query_authorship")])#Remove query_name and query_authorship; keep "query_full_name" column, contains the exact query submitted to GBIF

  return(data)
}
