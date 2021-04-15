#helper function for taxon_update_alt

name_update_loop <- function(unique_taxon, status_feed, gbif_out_l){#update a single taxon name
  one_row_analysis <- FALSE
  gbif_new_key <- ""
  if (unique_taxon[3]==""){
    blank <- TRUE
  }else{
    blank <- FALSE
    gbif_out_df <- gbif_out_l[[unique_taxon[3]]]
    #gbif_out_df <- taxize::get_gbifid_(sci = unique_taxon[3], method = "backbone", messages=status_feed)[[unique_taxon[3]]]
  }
  if (nrow(gbif_out_df) == 0 | blank==TRUE){ #No records returned from GBIF OR the query was blank (i.e. ""); blanks cause errors if submitted in get_gbif_ query
    unique_taxon[15] <- "error5"
  } else{ #One or more records returned from GBIF
    if (unique_taxon[2] != ""){#Author listed for input record
      gbif_out_row <- gbif_out_df[1, ]#Take the first (best) match returned from GBIF
      if (gbif_out_row$matchtype == "HIGHERRANK"){#Check that best match is not a HIGHERRANK taxon match
        unique_taxon[15] <- "error8"
      }else {
        one_row_analysis <- TRUE
      }
    }else {#No author listed for input record
      gbif_out_rows <- gbif_out_df[gbif_out_df$matchtype == "EXACT", ]#Check for EXACT matches
      if (nrow(gbif_out_rows) == 0){#No EXACT matches
        if (nrow(gbif_out_df[gbif_out_df$matchtype != "HIGHERRANK", ])==0){#All non-EXACT matches are HIGHERRANK-can't update name
          unique_taxon[15] <- "error2"
        }else{#Some non-exact matches are FUZZY, can proceed with name update
          gbif_out_rows <- gbif_out_df[gbif_out_df$matchtype != "HIGHERRANK", ]#Remove HIGHERRANK matches and keep FUZZY matches
        }
      }
      if (nrow(gbif_out_rows)==1){#one match, can be FUZZY or EXACT
        one_row_analysis <- TRUE
        gbif_out_row <- gbif_out_rows
      }
      if (nrow(gbif_out_rows)>1) {#more than one match, can be all FUZZY or all EXACT
        for (j in 1:nrow(gbif_out_rows)){#Check if all matches lead to the same valid or updated name
          if (gbif_out_rows$status[j] == "ACCEPTED"){ #Record has ACCEPTED status
            gbif_out_rows$new_key[j] <- gbif_out_rows$usagekey[j] #New key the same as "old" key
          } else {
            gbif_out_rows$new_key[j] <- ifelse(gbif_out_rows$status[j] == "DOUBTFUL","",gbif_out_rows$acceptedusagekey[j])#check if record has DOUBTFUL status(No new key) or SYNONYM status(New key is the key of the SYNONYM taxon)
          }
        }
        if (length(unique(gbif_out_rows$new_key)) == 1) {#New key the same for each match
          if (unique(gbif_out_rows$new_key) == ""){ #All matches had DOUBTFUL status, so no new key - can't update name
            unique_taxon[15] <- "error3"
          }else{#Matches were either ACCEPTED or SYNONYM status and resulted in one unique new key
            if (nrow(gbif_out_rows[gbif_out_rows$status== "ACCEPTED",])>0){#When matches include ACCEPTED record
              one_row_analysis <- TRUE
              gbif_out_row <- gbif_out_rows[gbif_out_rows$status== "ACCEPTED",]
            }else{#When matches are all SYNONYM status
              gbif_new_key <- gbif_out_rows$new_key[1]
              gbif_out_row <- gbif_out_rows[1,]#Get matchtype and taxon_conf info from best (highest conf) SYNONYM match
            }
          }
        } else {#Matches have different new keys - can't update name; unless...
          if (force_accepted == T & "ACCEPTED" %in% gbif_out_rows$status){#pick ACCEPTED gbif match and proceed with name update regardless of how "accurate" the match really is
            one_row_analysis <- TRUE
            gbif_out_row <- gbif_out_rows[gbif_out_rows$status == "ACCEPTED",][1,]
          }else{#pick_accpted option is FALSE; won't update name
            unique_taxon[15] <- "error4"
          }

        }
      }
    }
  }
  if (one_row_analysis == TRUE){
    if (gbif_out_row$status == "ACCEPTED"){ #Don't need to go to "new Key" gbif page
      unique_taxon[4] <- gbif_out_row$scientificname
      unique_taxon[5] <- gbif_out_row$canonicalname
      unique_taxon[6] <- gsub(paste(gbif_out_row$canonicalname, " ", sep = ""), "", gbif_out_row$scientificname)
      unique_taxon[7] <- gbif_out_row$kingdom
      unique_taxon[8] <- ifelse(is.null(gbif_out_row$phylum) == F,gbif_out_row$phylum,"")
      unique_taxon[9] <- ifelse(is.null(gbif_out_row$class) == F,gbif_out_row$class,"")
      unique_taxon[10] <- ifelse(is.null(gbif_out_row$order) == F,gbif_out_row$order,"")
      unique_taxon[11] <- ifelse(is.null(gbif_out_row$family) == F,gbif_out_row$family,"")
      unique_taxon[12] <- ifelse(is.null(gbif_out_row$genus) == F,gbif_out_row$genus,"")
      unique_taxon[13] <- gbif_out_row$confidence
      unique_taxon[14] <- gbif_out_row$matchtype

    } else {# status is not accepted
      if (gbif_out_row$status == "DOUBTFUL"){
        unique_taxon[15] <- "error1"
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
      unique_taxon[15] <- "error6"
    } else {
      if (key_record$taxonomicStatus == "SYNONYM"){#New name is still a synonym (likely indicates an error within GBIF) - can't update name
        unique_taxon[15] <- "error7"
      }else{#New name is ACCEPTED
        unique_taxon[4] <- key_record$scientificName
        unique_taxon[5] <- key_record$canonicalName
        unique_taxon[6] <- gsub(paste(key_record$canonicalName, " ", sep = ""), "", key_record$scientificName)
        unique_taxon[7] <- key_record$kingdom
        unique_taxon[8] <- ifelse(is.null(key_record$phylum) == F,key_record$phylum,"")
        unique_taxon[9] <- ifelse(is.null(key_record$class) == F,key_record$class,"")
        unique_taxon[10] <- ifelse(is.null(key_record$order) == F,key_record$order,"")
        unique_taxon[11] <- ifelse(is.null(key_record$family) == F,key_record$family,"")
        unique_taxon[12] <- ifelse(is.null(key_record$genus) == F,key_record$genus,"")
        unique_taxon[13] <- gbif_out_row$confidence
        unique_taxon[14] <- gbif_out_row$matchtype
      }
    }
  }
  return(unique_taxon[3:15])
}
