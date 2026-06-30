#' @title Clean and harmonize taxon names
#'
#' @description
#' Cleans and updates taxon names of fungi and plants based on \href{https://www.catalogueoflife.org/}{Catalogue of Life} data. 
#' Synonyms are updated to currently accepted names and current taxonomic hierarchies are assigned.
#'
#' @param data Data.frame.
#' @param name_col Character. Name of column containing taxon names.
#' @param author_col Character. Name of column containing author names.
#' @param kingdom Character. "Fungi" or "Plantae". Used to speed up taxon matching. Default: "Fungi".
#' @param refresh_db Logical. Should the COL database be refreshed. Default: FALSE. Note, database is always downloaded during the first execution of this function after package installation.
#' @param db_url Character. URL for COL database. Default: `https://download.catalogueoflife.org/col/annual/2025_dwca.zip`.
#'
#' @return Input data.frame with the following output fields appended:
#'
#' \describe{
#' \item{\code{kingdom_pres}}{Character. Accepted kingdom at present.}
#' \item{\code{phylum_pres}}{Character. Accepted phylum at present.}
#' \item{\code{class_pres}}{Character. Accepted class at present.}
#' \item{\code{order_pres}}{Character. Accepted order at present.}
#' \item{\code{family_pres}}{Character. Accepted family at present.}
#' \item{\code{genus_pres}}{Character. Accepted genus at present.}
#' \item{\code{specific_epithet_pres}}{Character. Accepted specific epithet at present.}
#' \item{\code{species_pres}}{Character. Accepted species at present.}
#' \item{\code{species_authority_pres}}{Character. Accepted authority at present.}
#' \item{\code{match_type}}{Character. 'EXACT' vs 'FUZZY'. See full description below.}
#' \item{\code{match_score}}{Integer. Confidence score (0-100) for match quality.}
#' }
#' @note Match type may include: 'EXACT-FULL' when full taxon name (species AND 
#' authority) is matched perfectly with record in COL data, 'EXACT-PARTIAL' 
#' when species name without authority is matched perfectly with record in COL 
#' data, 'FUZZY-FULL' when full taxon name (species AND authority) is fuzzy 
#' matched (based on string alignment algorithms) with 
#' record in COL data, FUZZY-PARTIAL' when species name without authority is 
#' fuzzy matched with record in COL data. 'FUZZY' matching is done when 'EXACT' 
#' matches are not found and come with a match score indicating match quality. 
#' Fuzzy matching is done for both full and partial names and whichever 
#' produces the highest match score is reported.
#'  
#' @export
#'
#' @examples
#' library(fungarium)
#' data(fomitopsidaceae) #import sample data set
#' clean_tax <- clean_taxonomy(fomitopsidaceae) #clean taxonomy
#'

clean_taxonomy <- function(data, name_col = "scientificName", 
                           author_col = "scientificNameAuthorship", 
                           kingdom = "Fungi", 
                           refresh_db = FALSE,
                           db_url = "https://download.catalogueoflife.org/col/annual/2025_dwca.zip", 
                           threads = 1L){
  # check args
  checkmate::assert_data_frame(data)
  checkmate::assertCharacter(name_col, max.len = 1)
  checkmate::assert_choice(name_col, choices=colnames(data))
  checkmate::assertCharacter(author_col, max.len = 1)
  checkmate::assert_choice(author_col, choices=colnames(data))
  checkmate::assert_character(kingdom, max.len = 1)
  checkmate::assert_choice(kingdom, c("Fungi", "Plantae"))
  checkmate::assert_logical(refresh_db, max.len = 1)
  checkmate::assert_character(db_url, max.len = 1)
  checkmate::assert_integer(threads, max.len = 1, lower = 1)

  # col files
  fungi_file <- cache_file("Taxon_fungi_w_tax_hier.tsv")
  plant_file <- cache_file("Taxon_plantae_w_tax_hier.tsv")

  # download fresh COL data
  if (!file.exists(fungi_file)||!file.exists(plant_file)||refresh_db){

    # Download the zip file
    cat("Downloading COL taxonomy db...\n")
    download.file(db_url, destfile = cache_file("col_dwca.zip"), mode = "wb")
    cat("Unzipping COL taxonomy db...\n")
    unzip(cache_file("col_dwca.zip"), exdir = cache_file("col_dwca"))
    unlink(cache_file("col_dwca.zip"))

    # load and clean
    cat("Processing COL taxonomy db...\n")
    col_data_file <- cache_file("col_dwca/Taxon.tsv")
    col_data <- data.table::fread(col_data_file, sep="\t", quote="", data.table = F, nThread = threads)
    unlink(cache_file("col_dwca"), recursive = TRUE)
    fungi <- select_kingdom(col_data, "Fungi")
    # fungi <- assign_tax_hier(fungi)
    data.table::fwrite(fungi, fungi_file, sep = '\t', quote = FALSE, nThread = threads)

    plants <- select_kingdom(col_data, "Plantae")
    # plants <- assign_tax_hier(plants)
    data.table::fwrite(plants, plant_file, sep = '\t', quote = FALSE, nThread = threads)

    if (kingdom=="Fungi"){
      col_data <-  fungi
    }else{
      col_data <-  plants
    }
    rm(fungi)
    rm(plants)
    gc()
  } else{
    # Load COL data
    if (kingdom=="Fungi"){
      col_data_file <- fungi_file
    } else{
      col_data_file <- plant_file
    }
    col_data <- data.table::fread(col_data_file, sep="\t", quote="", data.table = F, nThread = threads, showProgress = FALSE)
  }


  # Call cpp function
  cat("Cleaning input taxon names...\n") # TODO print number of unique names in input data
  data <- cbind(data, clean_taxonomy_cpp(data[[name_col]], data[[author_col]], col_data, threads))

  # return output
  return(data)
}


# helpers
select_kingdom <- function(col_data, kingdom){
  taxon_ids <- col_data[col_data$`dwc:scientificName`==kingdom,]$`dwc:taxonID` # kingdom
  all_ids <- taxon_ids
  while (length(taxon_ids)>0){
    taxon_ids <- col_data[col_data$`dwc:parentNameUsageID`%in%taxon_ids,]$`dwc:taxonID`
    all_ids <- c(all_ids, taxon_ids) 
  }
  all_ids <- c(all_ids, col_data[col_data$`dwc:acceptedNameUsageID`%in%all_ids,]$`dwc:taxonID`) # synonyms
  return(col_data[col_data$`dwc:taxonID`%in%all_ids,])
}


cache_file <- function(file_name) {
  dir <- tools::R_user_dir("fungarium", which = "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, file_name)
}