#' Assign fips codes
#'
#' Assigns fips codes to states or counties.
#'
#'
#' @param df  Dataframe containing a column with state names and (optionally) a column of county names.
#' @param state_col Character string specifying state column name. Defaults to "stateProvince" (name used by MyCoPortal).
#' @param county_col Character string specifying county column name. Defaults to "county" (name used by MyCoPortal).
#' @param counties Logical. If TRUE fips codes are returned for states AND counties. If FALSE, only state fips codes are returned.
#'
#' @return Returns input dataframe with fips codes appended:
#' \item{state_fips}{Two digit state fips code.}
#' \item{county_fips}{Five digit county fips code.}
#' \item{state_matchtype}{}
#' \item{state_conf}{}
#' \item{county_matchtype}{}
#' \item{county_conf}{}
#' @details Fips codes are assigned based on reference data from...\cr
#' \cr
#' When assigning fips codes, approximate name matches are possible
#' when names have variable nomenclature (e.g. "Anoka", "Anoka Co.", "Anoka County") or are simply
#' mispelled (e.g "Florda"). See "Return" sections for ways to validate the quality of
#' approximate matches.
#' @export
#'
#' @examples
#'
get_fips <- function(df, state_col="stateProvince", county_col="county", counties=TRUE){
  if(!counties %in% c(TRUE, FALSE)){
   stop("counties must be TRUE or FALSE")
  }
  #states need to be processed for assigning fips to states or counties
  state_fips <- utils::read.csv(system.file("extdata", "state_fips.csv",
                                            package = "usmap"), colClasses = "character")
  state_fips$abbr <- tolower(state_fips$abbr)#make all abbr lowercase
  state_fips$full <- tolower(state_fips$full)#make all state names lowercase
  unique_states <- data.frame(states=unique(tolower(df[[state_col]])))#make a list of unique states in fungi file then make them all lowercase
  unique_states$match_type <- "NA"
  for(i in 1:nrow(unique_states)){#assign fips to each unique state in fungi dataset
    if (unique_states$states[i] %in% c("washington, dc", "washington, d.c.", "washington dc", "washington d.c.")){#Manually ID variable DC spellings to avoid incorrect ID as Washington state
      unique_states$state_fips[i] <- state_fips[state_fips$abbr=="dc",]$fips
      unique_states$state_conf[i] <- 100
    }else{#all states other than variable DC spellings
      fips_row <- state_fips[state_fips$abbr == unique_states$states[i]|state_fips$full == unique_states$states[i],]
      if(nrow(fips_row)>0){#found exact match for state name or abbr
        unique_states$state_fips[i] <- fips_row$fips
        unique_states$state_conf[i] <- 100
      }else{#no exact match; must use approximate matching
        if(unique_states$states[i]==""){#state is blank
          unique_states$state_fips[i] <- ""
          unique_states$state_conf[i] <- 0
        }else{#state is not blank
          grep_match <- data.frame(state=state_fips$full, fips=state_fips$fips, match="")#search for all state names in the state "query string"; hels properly ID listed state names like "Southern Michigan" that would otherwise be poorly matched using string alignment algorithms
          for (j in 1:nrow(state_fips)){
            grep_match$match[j] <- length(grep(state_fips$full[j],unique_states$states[i]))
          }
          if (nrow(grep_match[grep_match$match==1,])>1){
            unique_states$state_fips[i] <- ""
            unique_states$state_conf[i] <- 0
          }else{
            if (nrow(grep_match[grep_match$match==1,])==1){
            unique_states$state_fips[i] <- grep_match[grep_match$match==1,]$fips
            unique_states$state_conf[i] <- 100
            }else{
            if (nchar(unique_states$states[i])==2){#get scores for state abbrevations
              state_scores <- as.data.frame(NameNeedle::needleScores(unique_states$states[i], state_fips$abbr, NameNeedle::defaultNeedleParams))
            }else{#get scores for full state names
              state_scores <- as.data.frame(NameNeedle::needleScores(unique_states$states[i], state_fips$full, NameNeedle::defaultNeedleParams))#records with more than one state listed may not be resolved properly.
            }
            colnames(state_scores) <- "scores"
            state_scores$states <- rownames(state_scores)
            if (nrow(state_scores[state_scores$scores>0,])==0){#check if any scores are greater than 0; if not, no fips assigned
              unique_states$state_fips[i] <- ""
              unique_states$state_conf[i] <- 0
            }else{
              max_row <- state_scores[state_scores$scores==max(state_scores$scores, na.rm = TRUE),]
              if (nrow(max_row)>1){#check if there are tied states; if so, no fips assigned
                unique_states$state_fips[i] <- ""
                unique_states$state_conf[i] <- 0
              }else{
                unique_states$state_fips[i] <- state_fips[state_fips$abbr==max_row$states|state_fips$full==max_row$states,]$fips
                unique_states$state_conf[i] <- max_row$scores/nchar(unique_states$states[i])*100
                unique_states$match_type[i] <- "Approx"
              }
            }
          }
          }
        }
      }
    }
  }
  for(i in 1:nrow(df)){#input the ID'd fips back into the original fungi file
    df$match_type[i] <- unique_states[unique_states$states==tolower(df[[state_col]][i]),]$match_type
    df$state_fips[i] <- unique_states[unique_states$states==tolower(df[[state_col]][i]),]$state_fips
    df$state_conf[i] <- as.integer(unique_states[unique_states$states==tolower(df[[state_col]][i]),]$state_conf)
  }
  if(counties==TRUE){
    county_fips <- utils::read.csv(system.file("extdata", "us_counties_centroids.csv",
                                               package = "usmap"), colClasses = "character")
    county_fips$county <- tolower(county_fips$county)#make all county names lowercase
    county_fips$county <- gsub(" county", "", county_fips$county)
    county_fips$county <- gsub(" parish", "", county_fips$county)
    county_fips$county <- gsub(" borough", "", county_fips$county)
    county_fips$county <- gsub(" census area", "", county_fips$county)
    county_fips$state_fips <- gsub("...$", "", county_fips$fips)
    unique_counties <- dplyr::select(df, county_col, state_fips)#make a list of unique counties in fungi file then make them all lowercase
    colnames(unique_counties) <- c("county", "state_fips")
    unique_counties$county_fixed <- tolower(unique_counties$county)
    unique_counties$county_fixed <- gsub(" county", "", unique_counties$county_fixed)
    unique_counties$county_fixed <- gsub(" co\\.", "", unique_counties$county_fixed)
    unique_counties$county_fixed <- gsub(" parish", "", unique_counties$county_fixed)
    unique_counties$county_fixed <- gsub(" borough", "", unique_counties$county_fixed)
    unique_counties$county_fixed <- gsub(" census area", "", unique_counties$county_fixed)
    unique_counties$county_fixed <- gsub(" $", "", unique_counties$county_fixed)
    unique_counties <- dplyr::distinct(unique_counties)
    unique_counties$match_type <- "NA"
    for(i in 1:nrow(unique_counties)){#assign fips to each unique county in fungi dataset
      fips_row <- county_fips[county_fips$county == unique_counties$county_fixed[i]&county_fips$state_fips==unique_counties$state_fips[i],]
      if(nrow(fips_row)>0){#found exact match for county name
        unique_counties$county_fips[i] <- fips_row$fips
        unique_counties$county_conf[i] <- 100
      }else{#no exact match; must use approximate matching
        if(unique_counties$county_fixed[i]==""|unique_counties$state_fips[i]==""){#county or state is blank
          unique_counties$county_fips[i] <- ""
          unique_counties$county_conf[i] <- 0
        }else{#county and state are not blank
          state_counties <- county_fips[county_fips$state_fips==unique_counties$state_fips[i],]
          county_scores <- as.data.frame(NameNeedle::needleScores(unique_counties$county_fixed[i], state_counties$county, NameNeedle::defaultNeedleParams))
          colnames(county_scores) <- "scores"
          county_scores$counties <- rownames(county_scores)
          if (nrow(county_scores[county_scores$scores>0,])==0){#check if any scores are greater than 0; if not, no fips assigned
            unique_counties$county_fips[i] <- ""
            unique_counties$county_conf[i] <- 0
          }else{
            max_row <- county_scores[county_scores$scores==max(county_scores$scores, na.rm = TRUE),]
            if (nrow(max_row)>1){#check if there are tied counties; if so, no fips assigned
              unique_counties$county_fips[i] <- ""
              unique_counties$county_conf[i] <- 0
            }else{
              unique_counties$match_type[i] <- "Approx"
              unique_counties$county_conf[i] <- max_row$scores/nchar(unique_counties$county_fixed[i])*100
              unique_counties$county_fips[i] <- county_fips[county_fips$county==max_row$counties&county_fips$state_fips==unique_counties$state_fips[i],]$fips
            }
          }
        }
      }
    }
    for(i in 1:nrow(df)){#input the ID'd fips back into the original fungi file
      df$match_type[i] <- unique_counties[unique_counties$county==df[[county_col]][i]&unique_counties$state_fips==df$state_fips[i],]$match_type
      df$county_fips[i] <- unique_counties[unique_counties$county==df[[county_col]][i]&unique_counties$state_fips==df$state_fips[i],]$county_fips
      df$county_conf[i] <- as.integer(unique_counties[unique_counties$county==df[[county_col]][i]&unique_counties$state_fips==df$state_fips[i],]$county_conf)
    }
  }
  return(df)
}
