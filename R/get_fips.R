#' Fix mispelled names and assign fips codes to US states and counties
#'
#' Assigns fips codes to US states or counties/county-equivalents. When a name is mispelled,
#' a fips code and the correctly spelled name is assigned using approximate
#' name matching algorithms.
#'
#' @param data  Dataframe containing a column with state names and (optionally) a column of county names.
#' @param state_col Character string specifying state column name. Defaults to "stateProvince" (name used by MyCoPortal).
#' @param county_col Character string specifying county column name. Defaults to "county" (name used by MyCoPortal).
#' @param assign_counties Logical. If TRUE, fips codes are assigned for states AND counties. If FALSE, only state fips codes are assigned.
#'
#' @return Returns input dataframe with the following output fields appended:
#' \item{state_name}{Character string. Matched state name.}
#' \item{state_fips}{Character string. Two digit state fips code.}
#' \item{state_matchtype}{Character string.
#' EXACT: a state name was matched exactly to the state listed in the fungal dataset;
#' PARTIAL: a state name was matched partially to the state listed in the fungal dataset;
#' MISPELLED: a state name was matched approximately to the mispelled state listed in the fungal dataset;
#' NONE: a state name could not be matched to the state listed in the fungal dataset.}
#' \item{state_conf}{Integer. The confidence score when a mispelled state name is approximately matched (0-100).
#' Names with NONE, EXACT, or PARTIAL matchtypes all get a score of 100.}
#' \item{county_name}{Character string. Matched county name.}
#' \item{county_fips}{Character string. Five digit county fips code.}
#' \item{county_matchtype}{Character string.
#' EXACT: a county name was matched exactly to the county listed in the fungal dataset;
#' PARTIAL: a county name was matched partially to the county listed in the fungal dataset;
#' MISPELLED: a county name was matched approximately to the mispelled county listed in the fungal dataset;
#' NONE: a county name could not be matched to the county listed in the fungal dataset.}
#' \item{county_conf}{Integer. Confidence score when a mispelled county name is approximately matched (0-100).
#' Names with NONE, EXACT, or PARTIAL matchtypes all get a score of 100.}
#'
#' @details Fips codes are assigned based on 2019 reference data from the
#' \href{https://www.census.gov/geographies/reference-files/2019/demo/popest/2019-fips.html}{US Census Bureau}.
#' County data includes counties and county-equivalents (e.g. parishes, boroughs, census areas, independent cities)
#' for the District of Columbia, Puerto Rico, and all 50 US states.\cr
#' \cr
#' When assigning fips codes, approximate name matches are possible
#' when names have variable nomenclature (e.g. "Anoka", "Anoka Co.", "Anoka County") or are simply
#' mispelled (e.g "Florda"). See "Value" section for validating the quality of
#' approximate matches.\cr
#' \cr
#' Only current county/county-equivalent names (as of December 2020) are used for assigning fips.
#' Fungal records from counties that had substaintially different names in the past or
#' counties that no longer exist (e.g. Bedford City) may not have fips codes assigned.
#' The only way to circumvent this issue is to include ALL historical names for every county in the
#' reference dataset, which currently has not been done.
#'
#' @export
#'
#' @examples
#' MP_data <- mycoportal_tab("Pleurotus") #Retrieve fungal data from MyCoPortal
#' MP_data <- get_fips(MP_data) #Assign fips codes to counties listed in fungal records
#'
get_fips <- function(data, state_col="stateProvince", county_col="county", assign_counties=TRUE){
  #check that the input is formatted correctly. If not, stop, and print an error.
  if (!is.data.frame(data)){
    stop('Input data needs to be a data.frame.')
  }
  #states need to be processed for assigning fips to states or counties
  state_fips <- utils::read.csv(system.file("extdata", "state_fips.csv",
                                            package = "fungarium"), colClasses = "character")#file contains all US states and territories
  state_fips$full_1 <- state_fips$full #make column for original state names before making lowercase
  state_fips$abbr <- tolower(state_fips$abbr)#make all abbreviations lowercase
  state_fips$full <- tolower(state_fips$full)#make all state names lowercase
  unique_states <- data.frame(states=unique(tolower(data[[state_col]])))#make a list of unique states in fungi file then make them all lowercase
  unique_states$state_matchtype <- "NONE"
  unique_states$state_name <- ""
  unique_states$state_fips <- ""
  unique_states$state_conf <- 100
  for(i in 1:nrow(unique_states)){#assign fips to each unique state in fungi dataset
    if (unique_states$states[i] %in% c("washington, dc", "washington, d.c.", "washington dc", "washington d.c.", "d.c.")){#Manually ID variable DC spellings to avoid incorrect ID as Washington state
      unique_states$state_fips[i] <- state_fips[state_fips$abbr=="dc",]$fips
      unique_states$state_name[i] <- state_fips[state_fips$abbr=="dc",]$full_1
      unique_states$state_conf[i] <- 100
      unique_states$state_matchtype[i] <- "EXACT"
    }else{#all states other than variable DC spellings
      fips_row <- state_fips[state_fips$abbr == unique_states$states[i]|state_fips$full == unique_states$states[i],]
      if(nrow(fips_row)>0){#found exact match for state name or abbr
        unique_states$state_fips[i] <- fips_row$fips
        unique_states$state_name[i] <- fips_row$full_1
        unique_states$state_matchtype[i] <- "EXACT"
      }else{#no exact match; must use approximate matching
        if(unique_states$states[i]==""){#state is blank; can't assign fips
        }else{#state is not blank
          grep_match <- state_fips[state_fips$full %in% grep(paste("\\Q",unique_states$states[i], "\\E", sep=""), state_fips$full, value=T),]#check is query name can be found in states names
          if (nrow(grep_match)>1){#more than one partial match; can't assign fips
          }else{#zero or one partial matches
            if (nrow(grep_match)==1){#one partial match; assign fips
              unique_states$state_fips[i] <- grep_match$fips
              unique_states$state_name[i] <- grep_match$full_1
              unique_states$state_matchtype[i] <- "PARTIAL"
            }else{#zero partial matches; continue with other matching protocols
              grep_match <- data.frame(state_1=state_fips$full_1, state=state_fips$full, fips=state_fips$fips, match="")#search for all state names in the state "query string"; hels properly ID listed state names like "Southern Michigan" that would otherwise be poorly matched using string alignment algorithms
              for (j in 1:nrow(state_fips)){
                grep_match$match[j] <- length(grep(state_fips$full[j],unique_states$states[i]))
              }
              if (nrow(grep_match[grep_match$match==1,])>1){#More than one state name found in query string
              }else{#one or zero county names found in query string
                if (nrow(grep_match[grep_match$match==1,])==1){#Just one state name found in query string
                  unique_states$state_fips[i] <- grep_match[grep_match$match==1,]$fips
                  unique_states$state_name[i] <- grep_match[grep_match$match==1,]$state_1
                  unique_states$state_matchtype[i] <- "PARTIAL"
                }else{#zero state names found in query string; use string alignment algorithm
                  if (nchar(unique_states$states[i])==2){#get scores for state abbrevations
                    state_scores <- as.data.frame(NameNeedle::needleScores(unique_states$states[i], state_fips$abbr, NameNeedle::defaultNeedleParams))
                  }else{#get scores for full state names
                    state_scores <- as.data.frame(NameNeedle::needleScores(unique_states$states[i], state_fips$full, NameNeedle::defaultNeedleParams))#records with more than one state listed may not be resolved properly.
                  }
                  colnames(state_scores) <- "scores"
                  state_scores$states <- rownames(state_scores)
                  if (nrow(state_scores[state_scores$scores>0,])==0){#check if any scores are greater than 0; if not, no fips assigned
                  }else{#some scores are greater than 0
                    max_row <- state_scores[state_scores$scores==max(state_scores$scores, na.rm = TRUE),]
                    if (nrow(max_row)>1){#check if there are tied states; if so, no fips assigned
                    }else{#max state found; fips assigned
                      unique_states$state_fips[i] <- state_fips[state_fips$abbr==max_row$states|state_fips$full==max_row$states,]$fips
                      unique_states$state_name[i] <- state_fips[state_fips$abbr==max_row$states|state_fips$full==max_row$states,]$full_1
                      unique_states$state_conf[i] <- max_row$scores/nchar(unique_states$states[i])*100
                      unique_states$state_matchtype[i] <- "MISPELLED"
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  for(i in 1:nrow(data)){#input the ID'd fips back into the original fungi file
    data$state_name[i] <- unique_states[unique_states$states==tolower(data[[state_col]][i]),]$state_name
    data$state_fips[i] <- unique_states[unique_states$states==tolower(data[[state_col]][i]),]$state_fips
    data$state_matchtype[i] <- unique_states[unique_states$states==tolower(data[[state_col]][i]),]$state_matchtype
    data$state_conf[i] <- as.integer(unique_states[unique_states$states==tolower(data[[state_col]][i]),]$state_conf)
  }
  if(assign_counties==TRUE){
    county_fips <- utils::read.csv(system.file("extdata", "county_fips.csv",
                                               package = "fungarium"), colClasses = "character")#file contains all US counties (or county equivalents) in all US states and territories
    county_fips$county_1 <- county_fips$county #make column for original county names before making lowercase
    county_fips$county <- tolower(county_fips$county)#make all county names lowercase
    county_fips$county <- gsub(" county", "", county_fips$county)
    county_fips$county <- gsub(" parish", "", county_fips$county)
    county_fips$county <- gsub(" borough", "", county_fips$county)
    county_fips$county <- gsub(" census area", "", county_fips$county)
    county_fips$state_fips <- gsub("...$", "", county_fips$fips)
    unique_counties <- data.frame(county=data[[county_col]], state_fips=data$state_fips)#make a list of unique counties (i.e. county/state combos) in fungi file then make them all lowercase
    unique_counties$county_fixed <- tolower(unique_counties$county)
    unique_counties$county_fixed <- gsub(" county", "", unique_counties$county_fixed)
    unique_counties$county_fixed <- gsub(" co\\.", "", unique_counties$county_fixed)
    unique_counties$county_fixed <- gsub(" parish", "", unique_counties$county_fixed)
    unique_counties$county_fixed <- gsub(" borough", "", unique_counties$county_fixed)
    unique_counties$county_fixed <- gsub(" census area", "", unique_counties$county_fixed)
    unique_counties$county_fixed <- gsub(" $", "", unique_counties$county_fixed)
    unique_counties <- dplyr::distinct(unique_counties)
    unique_counties$county_matchtype <- "NONE"
    unique_counties$county_name <- ""
    unique_counties$county_fips <- ""
    unique_counties$county_conf <- 100
    for(i in 1:nrow(unique_counties)){#assign fips to each unique county in fungi dataset
      fips_row <- county_fips[county_fips$county == unique_counties$county_fixed[i]&county_fips$state_fips==unique_counties$state_fips[i],]
      if(nrow(fips_row)>0){#found exact match for county name
        unique_counties$county_fips[i] <- fips_row$fips
        unique_counties$county_name[i] <- fips_row$county_1
        unique_counties$county_matchtype[i] <- "EXACT"
      }else{#no exact match; must use approximate matching
        if(unique_counties$county_fixed[i]==""|unique_counties$state_fips[i]==""){#county or state is blank
        }else{#county and state are not blank
          state_counties <- county_fips[county_fips$state_fips==unique_counties$state_fips[i],]
          grep_match <- state_counties[state_counties$county %in% grep(paste("\\Q", unique_counties$county_fixed[i],"\\E", sep=""), state_counties$county, value=T),]#check is query name can be found in county names
          if (nrow(grep_match)>1){#more than one partial match; can't assign fips
          }else{#zero or one partial matches
            if (nrow(grep_match)==1){#one partial match; assign fips
              unique_counties$county_fips[i] <- grep_match$fips
              unique_counties$county_name[i] <- grep_match$county_1
              unique_counties$county_matchtype[i] <- "PARTIAL"
            }else{#zero partial matches; continue with other matching protocols
              grep_match <- data.frame(county_1=state_counties$county_1, county=state_counties$county, fips=state_counties$fips, match="")#search for all county names in the county "query string"; helps properly ID listed county names like BLANK that would otherwise be poorly matched using string alignment algorithms
              for (j in 1:nrow(state_counties)){
                grep_match$match[j] <- length(grep(state_counties$county[j],unique_counties$county_fixed[i]))
              }
              if (nrow(grep_match[grep_match$match==1,])>1){#More than one county name found in query string; can't assign fips
              }else{#one or zero county names found in query string
                if (nrow(grep_match[grep_match$match==1,])==1){#One county name found in query string
                  unique_counties$county_fips[i] <- grep_match[grep_match$match==1,]$fips
                  unique_counties$county_name[i] <- grep_match[grep_match$match==1,]$county_1
                  unique_counties$county_matchtype[i] <- "PARTIAL"
                }else{#Zero county names found in query string; use string alignment algorithm
                  county_scores <- as.data.frame(NameNeedle::needleScores(unique_counties$county_fixed[i], state_counties$county, NameNeedle::defaultNeedleParams))
                  colnames(county_scores) <- "scores"
                  county_scores$counties <- rownames(county_scores)
                  if (nrow(county_scores[county_scores$scores>0,])==0){#check if any scores are greater than 0; if not, no fips assigned
                  }else{#some scores are greater than zero
                    max_row <- county_scores[county_scores$scores==max(county_scores$scores, na.rm = TRUE),]
                    if (nrow(max_row)>1){#check if there are tied counties; if so, no fips assigned
                    }else{#there is one max county; fips can be assigned
                      unique_counties$county_matchtype[i] <- "MISPELLED"
                      unique_counties$county_conf[i] <- max_row$scores/nchar(unique_counties$county_fixed[i])*100
                      unique_counties$county_fips[i] <- county_fips[county_fips$county==max_row$counties&county_fips$state_fips==unique_counties$state_fips[i],]$fips
                      unique_counties$county_name[i] <- county_fips[county_fips$county==max_row$counties&county_fips$state_fips==unique_counties$state_fips[i],]$county_1
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    for(i in 1:nrow(data)){#input the ID'd fips back into the original fungi file
      data$county_name[i] <- unique_counties[unique_counties$county==data[[county_col]][i]&unique_counties$state_fips==data$state_fips[i],]$county_name
      data$county_fips[i] <- unique_counties[unique_counties$county==data[[county_col]][i]&unique_counties$state_fips==data$state_fips[i],]$county_fips
      data$county_matchtype[i] <- unique_counties[unique_counties$county==data[[county_col]][i]&unique_counties$state_fips==data$state_fips[i],]$county_matchtype
      data$county_conf[i] <- as.integer(unique_counties[unique_counties$county==data[[county_col]][i]&unique_counties$state_fips==data$state_fips[i],]$county_conf)
    }
  }
  return(data)
}
