#' Fix mispelled names and assign fips codes to US states and counties
#'
#' Assigns fips codes to US states/territories or counties/county-equivalents. When a name is mispelled,
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
#' for the District of Columbia, US territories, and all 50 US states.\cr
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
#' library(fungarium)
#' data(strophariaceae) #import sample dataset
#' MP_data <- get_fips(strophariaceae) #Fix mispelled counties and assign fips codes
#'
get_fips <- function(data, state_col="stateProvince", county_col="county", assign_counties=TRUE){
  #check that the input is formatted correctly
  if (!is.data.frame(data)){
    stop('Input data needs to be a data.frame.')
  }
  #check that there are records in input dataframe
  if (nrow(data)==0){
    stop('Error: no data in input data.frame.')
  }

#state name analysis
  #import state fips file
  state_fips <- utils::read.csv(system.file("extdata", "state_fips.csv",
                                            package = "fungarium"), colClasses = "character", encoding="latin1")#file contains all US states and territories

  #clean up state fips file
  state_fips$abbr_clean <- str_clean(state_fips$abbr, periods="")
  state_fips$full_clean <- str_clean(state_fips$full, periods="")

  #clean up input fungal records file - make reference file for orginal names corresponding to cleaned names
  state_ref <- data.frame(state=unique(data[[state_col]])) #get unique names among original names
  state_ref$state_clean <- str_clean(state_ref$state, periods="") #clean unique names

  #match states names in input data to state names in fips file then assign fips codes
  unique_states <- data.frame(state_clean=unique(state_ref$state_clean))#get unique state names
  unique_states$state_matchtype <- "NONE"
  unique_states$state_name <- ""
  unique_states$state_fips <- ""
  unique_states$state_conf <- 100
  for(i in 1:nrow(unique_states)){
    if(unique_states$state_clean[i]==""){#state is blank; can't assign fips
    }else if (unique_states$state_clean[i] %in% c("washington dc", "dc")){#Manually ID variable DC spellings to avoid incorrect ID as Washington state
      unique_states$state_fips[i] <- state_fips[state_fips$abbr_clean=="dc",]$fips
      unique_states$state_name[i] <- state_fips[state_fips$abbr_clean=="dc",]$full
      unique_states$state_conf[i] <- 100
      unique_states$state_matchtype[i] <- "EXACT"
    }else{#all states other than variable DC spellings
      fips_row <- state_fips[state_fips$abbr_clean == unique_states$state_clean[i]|state_fips$full_clean == unique_states$state_clean[i],]
      if(nrow(fips_row)>0){#found exact match for state name or abbr
        unique_states$state_fips[i] <- fips_row$fips
        unique_states$state_name[i] <- fips_row$full
        unique_states$state_matchtype[i] <- "EXACT"
      }else{#no exact match; must use approximate matching
        grep_match <- state_fips[state_fips$full_clean %in% grep(paste("\\Q",unique_states$state_clean[i], "\\E", sep=""), state_fips$full_clean, value=T),]#check is query name can be found in states names
        if (nrow(grep_match)>1){#more than one partial match; can't assign fips
        }else{#zero or one partial matches
          if (nrow(grep_match)==1){#one partial match; assign fips
            unique_states$state_fips[i] <- grep_match$fips
            unique_states$state_name[i] <- grep_match$full
            unique_states$state_matchtype[i] <- "PARTIAL"
          }else{#zero partial matches; continue with other matching protocols
            grep_match <- data.frame(state=state_fips$full, state_clean=state_fips$full_clean, fips=state_fips$fips, match="")#search for all state names in the state "query string"; hels properly ID listed state names like "Southern Michigan" that would otherwise be poorly matched using string alignment algorithms
            for (j in 1:nrow(state_fips)){
              grep_match$match[j] <- length(grep(state_fips$full_clean[j],unique_states$state_clean[i]))
            }
            if (nrow(grep_match[grep_match$match==1,])>1){#More than one state name found in query string
            }else{#one or zero county names found in query string
              if (nrow(grep_match[grep_match$match==1,])==1){#Just one state name found in query string
                unique_states$state_fips[i] <- grep_match[grep_match$match==1,]$fips
                unique_states$state_name[i] <- grep_match[grep_match$match==1,]$state
                unique_states$state_matchtype[i] <- "PARTIAL"
              }else{#zero state names found in query string; use string alignment algorithm
                if (nchar(unique_states$state_clean[i])==2){#get scores for state abbrevations
                  state_scores <- as.data.frame(NameNeedle::needleScores(unique_states$state_clean[i], state_fips$abbr_clean, NameNeedle::defaultNeedleParams))
                }else{#get scores for full state names
                  state_scores <- as.data.frame(NameNeedle::needleScores(unique_states$state_clean[i], state_fips$full_clean, NameNeedle::defaultNeedleParams))#records with more than one state listed may not be resolved properly.
                }
                colnames(state_scores) <- "scores"
                state_scores$state_clean <- rownames(state_scores)
                if (nrow(state_scores[state_scores$scores>0,])==0){#check if any scores are greater than 0; if not, no fips assigned
                }else{#some scores are greater than 0
                  max_row <- state_scores[state_scores$scores==max(state_scores$scores, na.rm = TRUE),]
                  if (nrow(max_row)>1){#check if there are tied states; if so, no fips assigned
                  }else{#max state found; fips assigned
                    unique_states$state_fips[i] <- state_fips[state_fips$abbr_clean==max_row$state_clean|state_fips$full_clean==max_row$state_clean,]$fips
                    unique_states$state_name[i] <- state_fips[state_fips$abbr_clean==max_row$state_clean|state_fips$full_clean==max_row$state_clean,]$full
                    unique_states$state_conf[i] <- max_row$scores/nchar(unique_states$state_clean[i])*100
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

  #merge dataframe that has assigned fips codes with reference file that contains original (uncleaned) state names listed in fungal records file
  state_ref <- dplyr::inner_join(state_ref, unique_states, by="state_clean")

  #put the ID'd fips codes back into the original fungi file
  for(i in 1:nrow(data)){
    data$state_name[i] <- state_ref[state_ref$state==data[[state_col]][i],]$state_name
    data$state_fips[i] <- state_ref[state_ref$state==data[[state_col]][i],]$state_fips
    data$state_matchtype[i] <- state_ref[state_ref$state==data[[state_col]][i],]$state_matchtype
    data$state_conf[i] <- as.integer(state_ref[state_ref$state==data[[state_col]][i],]$state_conf)
  }

#county analysis
  #import county fips file
  if(assign_counties==TRUE){
    county_fips <- utils::read.csv(system.file("extdata", "county_fips.csv",
                                               package = "fungarium"), colClasses = "character", encoding = "latin1")#file contains all US counties (or county equivalents) in all US states and territories

    #clean county names in fips file
    county_fips$county_clean <- str_clean(county_fips$county, periods="")
    county_fips$county_clean <- gsub(" county", "", county_fips$county_clean)
    county_fips$county_clean <- gsub(" parish", "", county_fips$county_clean)
    county_fips$county_clean <- gsub(" borough", "", county_fips$county_clean)
    county_fips$county_clean <- gsub(" census area", "", county_fips$county_clean)

    #extract state fips from county fips
    county_fips$state_fips <- gsub("...$", "", county_fips$fips)

    #clean fungal records file
    unique_counties <- data.frame(county=data[[county_col]], state_fips=data$state_fips)#make a list of unique counties (i.e. county/state combos) in fungi file then make them all lowercase
    unique_counties$county_clean <- str_clean(unique_counties$county, periods="")
    unique_counties$county_clean <- gsub(" county", "", unique_counties$county_clean)
    unique_counties$county_clean <- gsub(" co", "", unique_counties$county_clean)
    unique_counties$county_clean <- gsub(" parish", "", unique_counties$county_clean)
    unique_counties$county_clean <- gsub(" borough", "", unique_counties$county_clean)
    unique_counties$county_clean <- gsub(" census area", "", unique_counties$county_clean)

    #assign fips codes to each unique county in fungi dataset
    unique_counties <- dplyr::distinct(unique_counties)
    unique_counties$county_matchtype <- "NONE"
    unique_counties$county_name <- ""
    unique_counties$county_fips <- ""
    unique_counties$county_conf <- 100
    for(i in 1:nrow(unique_counties)){
      fips_row <- county_fips[county_fips$county_clean == unique_counties$county_clean[i]&county_fips$state_fips==unique_counties$state_fips[i],]
      if(nrow(fips_row)>0){#found exact match for county name
        unique_counties$county_fips[i] <- fips_row$fips
        unique_counties$county_name[i] <- fips_row$county
        unique_counties$county_matchtype[i] <- "EXACT"
      }else{#no exact match; must use approximate matching
        if(unique_counties$county_clean[i]==""|unique_counties$state_fips[i]==""){#county or state is blank
        }else{#county and state are not blank
          state_counties <- county_fips[county_fips$state_fips==unique_counties$state_fips[i],]
          grep_match <- state_counties[state_counties$county_clean %in% grep(paste("\\Q", unique_counties$county_clean[i],"\\E", sep=""), state_counties$county_clean, value=T),]#check is query name can be found in county names
          if (nrow(grep_match)>1){#more than one partial match; can't assign fips
          }else{#zero or one partial matches
            if (nrow(grep_match)==1){#one partial match; assign fips
              unique_counties$county_fips[i] <- grep_match$fips
              unique_counties$county_name[i] <- grep_match$county
              unique_counties$county_matchtype[i] <- "PARTIAL"
            }else{#zero partial matches; continue with other matching protocols
              grep_match <- data.frame(county=state_counties$county, county_clean=state_counties$county_clean, fips=state_counties$fips, match="")#search for all county names in the county "query string"; helps properly ID listed county names like BLANK that would otherwise be poorly matched using string alignment algorithms
              for (j in 1:nrow(state_counties)){
                grep_match$match[j] <- length(grep(state_counties$county_clean[j],unique_counties$county_clean[i]))
              }
              if (nrow(grep_match[grep_match$match==1,])>1){#More than one county name found in query string; can't assign fips
              }else{#one or zero county names found in query string
                if (nrow(grep_match[grep_match$match==1,])==1){#One county name found in query string
                  unique_counties$county_fips[i] <- grep_match[grep_match$match==1,]$fips
                  unique_counties$county_name[i] <- grep_match[grep_match$match==1,]$county
                  unique_counties$county_matchtype[i] <- "PARTIAL"
                }else{#Zero county names found in query string; use string alignment algorithm
                  county_scores <- as.data.frame(NameNeedle::needleScores(unique_counties$county_clean[i], state_counties$county_clean, NameNeedle::defaultNeedleParams))
                  colnames(county_scores) <- "scores"
                  county_scores$counties <- rownames(county_scores)
                  if (nrow(county_scores[county_scores$scores>0,])==0){#check if any scores are greater than 0; if not, no fips assigned
                  }else{#some scores are greater than zero
                    max_row <- county_scores[county_scores$scores==max(county_scores$scores, na.rm = TRUE),]
                    if (nrow(max_row)>1){#check if there are tied counties; if so, no fips assigned
                    }else{#there is one max county; fips can be assigned
                      unique_counties$county_matchtype[i] <- "MISPELLED"
                      unique_counties$county_conf[i] <- max_row$scores/nchar(unique_counties$county_clean[i])*100
                      unique_counties$county_fips[i] <- county_fips[county_fips$county_clean==max_row$counties&county_fips$state_fips==unique_counties$state_fips[i],]$fips
                      unique_counties$county_name[i] <- county_fips[county_fips$county_clean==max_row$counties&county_fips$state_fips==unique_counties$state_fips[i],]$county
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    #put the ID'd fips back into the original fungi file
    for(i in 1:nrow(data)){
      data$county_name[i] <- unique_counties[unique_counties$county==data[[county_col]][i]&unique_counties$state_fips==data$state_fips[i],]$county_name
      data$county_fips[i] <- unique_counties[unique_counties$county==data[[county_col]][i]&unique_counties$state_fips==data$state_fips[i],]$county_fips
      data$county_matchtype[i] <- unique_counties[unique_counties$county==data[[county_col]][i]&unique_counties$state_fips==data$state_fips[i],]$county_matchtype
      data$county_conf[i] <- as.integer(unique_counties[unique_counties$county==data[[county_col]][i]&unique_counties$state_fips==data$state_fips[i],]$county_conf)
    }
  }
  return(data)
}
