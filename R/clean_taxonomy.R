#' @title Clean and harmonize taxon names
#'
#' @description
#' Parses taxon names of fungi and plants based on \href{https://www.catalogueoflife.org/}{Catalogue of Life} data. 
#' Synonyms are updated to currently accepted names.
#'
#' @param data `Data.frame`.
#' @param name_col Character. Column containing taxon names.
#' @param author_col Character. Column containing author names.
#' @param kingdom Character. `Fungi` or `Plantae`. Used to speed up taxon matching. Default: `Fungi`.
#' @param refresh_db Logical. Should the COL database be refreshed. Default: `FALSE`. Note, database is always downloaded during the first execution of this function after package installation.
#' @param db_url Character. URL for COL database. Default: `https://download.catalogueoflife.org/col/annual/2024_dwca.zip`.
#'
#' @return Input `data.frame` with the following output fields appended.
#'
#' \describe{
#' \item{\code{kingdom_pres}}{Character. ...}
#' \item{\code{phylum_pres}}{Character. ...}
#' \item{\code{class_pres}}{Character. ...}
#' \item{\code{order_pres}}{Character. ...}
#' \item{\code{family_pres}}{Character. ...}
#' \item{\code{genus_pres}}{Character. ...}
#' \item{\code{specific_epithet_pres}}{Character. ...}
#' \item{\code{species_pres}}{Character. ...}
#' \item{\code{species_authority_pres}}{Character. ...}
#' \item{\code{match_type}}{Character. ...}
#' \item{\code{match_score}}{Integer. ...}
#' \item{\code{taxon_raw}}{Character. ...}
#' \item{\code{taxon_authority_raw}}{Character. ...}
#' }
#' @note ...
#' @export
#'
#' @examples
#' library(fungarium)
#' data(agaricales) #import sample data set
#' clean_tax <- clean_taxonomy(agaricales) #clean taxonomy
#'

clean_taxonomy <- function(data, name_col = "scientificName", 
                           author_col = "scientificNameAuthorship", 
                           kingdom = "Fungi", 
                           refresh_db = FALSE,
                           db_url = "https://download.catalogueoflife.org/col/annual/2024_dwca.zip", 
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