#' Get trait enrichment factors
#'
#' Calculates trait enrichment factors (i.e. trait records/all records) for each unique element of
#' a specified variable (e.g. taxon, location, year, etc).
#'
#'
#' @param all_rec Data.frame of fungal occurrence records in Darwin Core format
#' @param trait_rec Data.frame of records within all_rec that are associated with the trait of interest. See: \code{find_trait}
#' @param by Character string specifying the variable in the all_rec/trait_rec data sets, for which enrichment factors will be calculated. Default is "new_full_name" (i.e. full taxon names).
#' @param ext_var Character vector specifying additional variables to keep with the "by" variable in output data frame. Useful for retaining taxonomic hierarchy information for species. Note that "by" variable elements must only have one unique set of values for ext_vars.
#' @param coll_bias Logical. If TRUE, collector bias for total records and trait records is calculated for each variable element. Based on collector information in the "recordedBy" field of Darwin Core data sets.
#' @param cores Integer specifying number of cores to use for processing. Default is 1. Values greater than 1 utilize parallel processing (not allowed on Windows systems). Parallel processing not recommended for use in GUI setting. See \code{parallel::mclapply}.
#' @param status_feed Logical. If TRUE, status of collector bias analysis in printed to the console.
#'
#' @return Data.frame containing unique variable elements in the input data set (e.g. unique taxa) with the following output fields appended for each variable element.
#' \item{freq}{Numeric. Number of records in the full dataset}
#' \item{trait_freq}{Numeric. Number of records in the trait dataset}
#' \item{trait_ratio}{Numeric. trait_freq/freq}
#' \item{coll_blanks}{Numeric. Number of total records with blank collector info.}
#' \item{blanks_bias}{Numeric. Proportion of total records that have blank collector info.}
#' \item{coll_blanks_t}{Numeric. Number of trait records with blank collector info.}
#' \item{blanks_bias_t}{Numeric. Proportion of trait records that have blank collector info.}
#' \item{max_bias}{Numeric. Max proportion of total records associated with one collector group. Blanks are treated as a collector group.}
#' \item{coll_groups}{Numeric. Number of unique collector groups for total records. Blanks are treated as a collector group.}
#' \item{max_bias_t}{Numeric. Max proportion of trait records associated with one collector group. Blanks are treated as a collector group.}
#' \item{coll_groups_t}{Numeric. Number of unique collector groups for trait records. Blanks are treated as a collector group.}
#' @note Blanks are automatically removed, and are not treated as a unique variable element.
#' @details Collector bias calculations help determine which variable elements may
#' have biased or untrustworthy enrichment values. This is done by determining what
#' proportions of records were collected by one specific collector or associated
#' group of collectors (e.g., a research team making collections together). If
#' one variable element (e.g., species) was collected excessively by one collector
#' or collector group there is higher chance that the enrichment value may be skewed.
#' For example, if our enrichment value was based on association with fire-affected environments
#' and 90% of the records for one species were collected by one collector in a burned environment,
#' the fire-associated enrichment for that species will be high but highly biased.
#' If other collectors had also found this species in fire-affected environments we
#' may have greater confidence that this species does have a high fire-associated enrichment value.\cr
#' \cr
#' While collector bias calculations can be useful, there are some caveats. Most notably,
#' collector information is not always reported, and this may be heavily location dependent
#' (e.g., some countries like the UK or Japan don't seem to report collector names).
#' In these scenarios, it may be impossible to determine accurate bias values because
#' collector information is missing. If you suspect that your data does not have
#' consistent collector information (e.g., "recordedBy" field in Darwin Core data sets)
#' you should use the collector bias analysis with caution.
#' @import data.table
#' @export
#'
#' @examples
#' library(fungarium)
#' data(agaricales_updated) #import sample data set with updated taxon names
#'
#' #apply filters
#' agaricales_updated <- agaricales_updated[agaricales_updated$error=="",]
#' agaricales_updated <- agaricales_updated[agaricales_updated$occurrenceRemarks!=""|agaricales_updated$habitat!=""|agaricales_updated$associatedTaxa!="",]
#'
#' #Finds fire-associated records
#' string1 <- "(?i)charred|burn(t|ed)|scorched|fire.?(killed|damaged|scarred)|killed.by.fire"
#'
#' #Removes records falsely identified as fire-associated
#' string2 <- "(?i)un.?burn(t|ed)"
#'
#' #find trait-relevant records
#' trait_rec <- find_trait(agaricales_updated,pos_string=string1, neg_string=string2)
#'
#' #get trait enrichment
#' trait_enrichment <- enrichment(all_rec=agaricales_updated, trait_rec=trait_rec, status_feed=FALSE, coll_bias=TRUE)
#'
#' #filter taxa based on collector bias (optional)
#' trait_enrichment <- trait_enrichment[trait_enrichment$max_bias<=0.75,]
#'
#' #filter taxa based on total number of records (optional)
#' trait_enrichment <- trait_enrichment[trait_enrichment$freq>=5,]

enrichment <- function(all_rec,
                       trait_rec,
                       ext_var=c("new_kingdom","new_phylum", "new_class",
                              "new_order", "new_family", "new_genus",
                              "new_species"),
                       by="new_full_name",
                       coll_bias=FALSE,
                       cores=1,
                       status_feed=FALSE){
  #check input data
  if (!is.data.frame(all_rec)){#check if data.frame/data.table
    stop("Input data must be a data.frame or data.table.")
  }
  if (!is.data.frame(trait_rec)){#check if data.frame/data.table
    stop("Input data must be a data.frame or data.table.")
  }

  #remove records where variable is blank
  all_rec <- all_rec[all_rec[[by]]!="",]
  trait_rec <- trait_rec[trait_rec[[by]]!="",]

  #condense input data - keep required columns only
  if (coll_bias){
    keep <- c(by,"recordedBy")
  }else{
    keep <- by
  }
  if (!is.null(ext_var)){#save ext_var key to reassign hier later
    ext_var_key <- dplyr::distinct(all_rec[,colnames(all_rec)%in%append(by, ext_var)])
  }
  all_rec <- all_rec[,colnames(all_rec)%in%keep, drop=F]
  trait_rec <- trait_rec[,colnames(trait_rec)%in%keep, drop=F]


  #Count the frequency of each unique variable element in the total dataset
  counts1 <- plyr::count(all_rec,vars=by)

  #Count the frequency of each unique variable element in trait dataset
  counts2 <- plyr::count(trait_rec,vars=by)
  colnames(counts2)[2] <- "trait_freq"

  #Merge total frequency and trait frequency data into one dataframe
  counts <- dplyr::left_join(counts1, counts2, by=by)
  rm(counts1, counts2)
  counts[is.na(counts$trait_freq),"trait_freq"] <- 0

  #Use total freq and trait freq to calculate enrichment factors
  counts$trait_ratio <- counts$trait_freq/counts$freq

  #bias analysis
  if (coll_bias==F){
    if (!is.null(ext_var)){#reassign ext_var
      counts <- dplyr::left_join(counts, ext_var_key, by=by)
    }
    return(counts)
  }else{
    #convert to data.table; helps improve speed of bias analysis
    all_rec <- setDT(all_rec)
    trait_rec <- setDT(trait_rec)
    #blank check all records
    all_rec$colls_cleaned <- str_clean(all_rec$recordedBy,periods="", letter_thresh = 1)
    counts3 <- plyr::count(all_rec[all_rec$colls_cleaned=="",],vars=by)
    colnames(counts3)[2] <- "coll_blanks"

    counts <- dplyr::left_join(counts, counts3, by=by)
    rm(counts3)
    counts[is.na(counts$coll_blanks),"coll_blanks"] <- 0
    counts$blanks_bias <- counts$coll_blanks/counts$freq

    #blank check trait records
    trait_rec$colls_cleaned <- str_clean(trait_rec$recordedBy,periods="", letter_thresh = 1)
    counts3 <- plyr::count(trait_rec[trait_rec$colls_cleaned=="",],vars=by)
    colnames(counts3)[2] <- "coll_blanks_t"

    counts <- dplyr::left_join(counts, counts3, by=by)
    rm(counts3)
    counts[is.na(counts$coll_blanks_t),"coll_blanks_t"] <- 0
    counts$blanks_bias_t <- counts$coll_blanks_t/counts$trait_freq
    counts$blanks_bias_t <- fifelse(is.na(counts$blanks_bias_t), 0,counts$blanks_bias_t)

    #bias check
    ##add "easily" calculated bias values - all_rec
    low_all <- counts[counts$freq==1|counts$blanks_bias==1,]
    low_all$max_bias <- fifelse(low_all$freq==1, 1,-1)
    low_all$coll_groups <- fifelse(low_all$freq==1, 1,-1)
    low_all$max_bias <- fifelse(low_all$blanks_bias==1, 1,low_all$max_bias) #colls all blanks
    low_all$coll_groups <- fifelse(low_all$blanks_bias==1, 1,low_all$coll_groups) #colls all blanks

    ##add "easily" calculated bias values - trait_rec
    low_trait <- counts[counts$trait_freq<2|counts$blanks_bias_t==1,]
    low_trait$max_bias_t <- fifelse(low_trait$trait_freq==0, 0,-1)
    low_trait$coll_groups_t <- fifelse(low_trait$trait_freq==0, 0,-1)
    low_trait$max_bias_t <- fifelse(low_trait$trait_freq==1, 1,low_trait$max_bias_t)
    low_trait$coll_groups_t <- fifelse(low_trait$trait_freq==1, 1,low_trait$coll_groups_t)
    low_trait$max_bias_t <- fifelse(low_trait$blanks_bias_t==1, 1,low_trait$max_bias_t) #colls all blanks
    low_trait$coll_groups_t <- fifelse(low_trait$blanks_bias_t==1, 1,low_trait$coll_groups_t) #colls all blanks

    ##get collector bias for remaining records
    high_all <- counts[!counts[[by]]%in%low_all[[by]],]
    high_trait <- counts[!counts[[by]]%in%low_trait[[by]],]
    rm(counts)
    all_rec <- all_rec[all_rec[[by]]%in%high_all[[by]],]
    trait_rec <- trait_rec[trait_rec[[by]]%in%high_trait[[by]],]
    high_all <- get_coll_bias(enrich_data=setDT(high_all), records_data=setDT(all_rec), by, cores, status_feed, done=nrow(low_all), freq_col="freq") #bias analysis for all_rec
    counts_all <- rbind(low_all, high_all)

    high_trait <- get_coll_bias(enrich_data=setDT(high_trait), records_data=setDT(trait_rec), by, cores, status_feed, done=nrow(low_trait), freq_col="trait_freq") #bias analysis for trait_rec
    counts_trait <- rbind(low_trait, high_trait)
    counts_trait <- counts_trait[,c(by, "max_bias_t", "coll_groups_t")]
    counts_all <- dplyr::inner_join(counts_all, counts_trait , by=by)

    if (!is.null(ext_var)){#reassign ext_var
      counts_all <- dplyr::left_join(counts_all, ext_var_key, by=by)
    }
    return(counts_all)
  }
}
