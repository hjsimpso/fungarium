#' Get trait enrichment factors
#'
#' Calculates trait enrichment factors (i.e. trait records/all records) for each unique element of
#' a specified variable (e.g. taxon, location, time).
#'
#'
#' @param all_rec Data.frame/data.table of fungal records
#' @param trait_rec Data.frame/data.table of records within all_rec that are associated with the trait of interest. See: \code{find_trait}
#' @param by Character string specifying the variable in the all_rec/trait_rec datasets, for which enrichment factors will be calculated. Default is "new_full_name" (i.e. full taxon names).
#' @param cols Character vector specifiying which columns associated with the specified variable should be included in the output. Default is: c("new_name", "new_author", "new_kingdom","new_phylum", "new_class", "new_order","new_family", "new_genus","new_specific_epithet").
#' @param coll_bias Logical. If TRUE, collector bias for total records and trait records is calculated for each variable element.
#' @param cores Integer. Default is 1. Specifies number of cores to use for processing. Values greater than 1 utilize parallel processing (not allowed on Windows systems). Parallel processing not recommended for use in GUI setting. See \code{parallel::mclapply}.
#' @param status_feed Logical. If TRUE, status of collector bias analysis in printed to the console.
#'
#' @return Data.table containing unique variable elements in the input dataset (e.g. unique taxa) with the following output fields appended for each variable element.
#' Note that if \code{cols} is non-NULL, the specifying variables will be appended as well.
#' \item{freq}{Numeric. Number of records in the full dataset}
#' \item{trait_freq}{Numeric. Number of records in the trait dataset}
#' \item{trait_ratio}{Numeric. trait_freq/freq}
#' \item{max_bias}{Numeric. (max number of records associated with one collector group)/(total records). For full dataset.}
#' \item{coll_groups}{Numeric. Number of unique collector groups. For full dataset.}
#' \item{max_bias_t}{Numeric. (max number of records associated with one collector group)/(total records). For trait dataset.}
#' \item{coll_groups_t}{Numeric. Number of unique collector groups. For trait dataset.}
#' @details Blanks are automatically removed, and are not treated as a unique variable element.\cr
#' \cr
#' If calculating enrichment by taxon, note that some taxa may share the same canonical name, but not the same full name.
#' This seems to be an issue for varieties in particular. For example, 	"Galerina vittiformis f. vittiformis"
#' is currently accepted by GBIF (as of Mar. 12, 2021) and the accepted canonical name is "Galerina vittiformis vittiformis", while
#' "Galerina vittiformis var. vittiformis" is also accepted and shares the same canonical name; thus, enrichment factors
#' will vary depending on whether or not you use the full name or canonical name. However, if using the taxon update function with
#' \code{species_only} set to TRUE, sub species are automatically removed from the dataset.
#' @import data.table
#' @export
#'
#' @examples
#' library(fungarium)
#' data(strophariaceae) #import sample dataset
#' data <- taxon_update(strophariaceae, show_status=FALSE) #update taxon names
#'
#' #Finds fire-associated records
#' string1 <- "(?i)charred|burn(t|ed)|scorched|fire.?(killed|damaged|scarred)|killed.by.fire"
#'
#' #Removes records falsely identfied as fire-associated
#' string2 <- "(?i)un.?burn(t|ed)"
#'
#' #filter out records that do not contain any environmental metadata (optional)
#' data <- data[data$occurrenceRemarks!=""|data$host!=""|
#'                    data$habitat!=""|data$substrate!="",]
#'
#' #find trait-relevant records
#' trait_data <- find_trait(data, pos_string=string1, neg_string=string2)
#'
#' #get trait enrichment
#' trait_enrichment <- enrichment(all_rec=data, trait_rec=trait_data, status_feed=FALSE)
#'
#' #filter taxa based on collector bias (optional)
#' trait_enrichment <- trait_enrichment[trait_enrichment$max_bias<=0.75,]
#'
#' #filter taxa based on total number of records (optional)
#' trait_enrichment <- trait_enrichment[trait_enrichment$freq>=5,]

enrichment <- function(all_rec,
                       trait_rec,
                       by="new_full_name",
                       cols=c("new_name", "new_author", "new_kingdom",
                                                      "new_phylum", "new_class", "new_order",
                                                      "new_family", "new_genus",
                                                      "new_species"),
                       coll_bias=TRUE,
                       cores=1,
                       status_feed=TRUE){
  #check input data
  if (!is.data.frame(all_rec)){#check if data.frame/data.table
    stop("Input data must be a data.frame or data.table.")
  }
  if (!is.data.frame(trait_rec)){#check if data.frame/data.table
    stop("Input data must be a data.frame or data.table.")
  }
  if (!is.data.table(all_rec)){#check specifically if input is data.table
    message("Coercing input data to data.table.")
    all_rec <- setDT(all_rec)
  }
  if (!is.data.table(trait_rec)){#check specifically if input is data.table
    message("Coercing input data to data.table.")
    trait_rec <- setDT(trait_rec)
  }

  #remove records where variable is blank
  all_rec <- all_rec[all_rec[[by]]!="",]
  trait_rec <- trait_rec[trait_rec[[by]]!="",]

  #Count the frequency of each unique variable element in the total dataset
  counts1 <- plyr::count(all_rec,vars=append(by,cols))

  #Count the frequency of each unique variable element in trait dataset
  counts2 <- plyr::count(trait_rec,vars=by)
  colnames(counts2)[2] <- "trait_freq"

  #Merge total frequency and trait frequency data into one dataframe
  counts <- merge(counts1, counts2, by.x=by,
                  by.y=by, all.x=T, all.y=T)
  counts[is.na(counts$trait_freq),"trait_freq"] <- 0

  #Use total freq and trait freq to calculate enrichment factors
  counts$trait_ratio <- counts$trait_freq/counts$freq

  #bias analysis
  if (coll_bias==F){
    return(counts)
  }else{
    counts <- get_coll_bias(data.table(counts), all_rec, trait_rec, by, cores, status_feed)
    return(counts)
  }
}
