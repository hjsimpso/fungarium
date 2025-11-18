#' @title Clean and harmonize species names
#'
#' @description
#' Parses species names...
#'
#' @param data `dwca` object.
#' @param kingdom Character. `Fungi` or `Plantae`. Used to speed up taxon matching. Default: `Fungi`.
#' @param refresh_db Logical. Should the COL database be refreshed. Default: `FALSE`. Note, database is always downloaded during the first execution of this function after package installation.
#' @param db_url Character. URL for COL database. Default: `https://download.catalogueoflife.org/col/annual/2024_dwca.zip`.
#'
#' @return Input `dwca` with the following output fields appended.
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
#' clean_tax <- clean_taxonomy(as_dwca(agaricales)) #clean taxonomy
#'

clean_taxonomy <- function(data, kingdom = "Fungi", refresh_db = FALSE,
                           db_url = "https://download.catalogueoflife.org/col/annual/2024_dwca.zip", threads = 1L){
  # check args
  if (!inherits(data, "dwca")) {
    stop("'data' must be of class 'dwca'. Use `as_dwca()` first.")
  }
  checkmate::assert_character(kingdom, max.len = 1)
  checkmate::assert_choice(kingdom, c("Fungi", "Plantae"))
  checkmate::assert_logical(refresh_db, max.len = 1)
  checkmate::assert_character(db_url, max.len = 1)
  checkmate::assert_integer(threads, max.len = 1, lower = 1)
  
  # get attributes
  input_attributes <- attributes(data)

  # col files
  fungi_file <- "inst/extdata/Taxon_fungi_w_tax_hier.tsv"
  plant_file <- "inst/extdata/Taxon_plantae_w_tax_hier.tsv"


  # download fresh COL data
  if (!file.exists(fungi_file)||!file.exists(plant_file)||refresh_db){

    # Download the zip file
    cat("Downloading COL taxonomy db...\n")
    download.file(db_url, destfile = "col_dwca.zip", mode = "wb")
    cat("Unzipping COL taxonomy db...\n")
    unzip("col_dwca.zip", exdir = "col_dwca")
    unlink("col_dwca.zip")

    # load and clean
    cat("Processing COL taxonomy db...\n")
    col_data_file <- "col_dwca/Taxon.tsv"
    col_data <- data.table::fread(col_data_file, sep="\t", quote="", data.table = F)
    unlink("col_dwca", recursive = TRUE)
    fungi <- select_kingdom(col_data, "Fungi")
    fungi <- assign_tax_hier(fungi)
    data.table::fwrite(fungi, fungi_file, sep = '\t', quote = FALSE)

    plants <- select_kingdom(col_data, "Plantae")
    plants <- assign_tax_hier(plants)
    data.table::fwrite(plants, plant_file, sep = '\t', quote = FALSE)

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
  data <- cbind(data, clean_taxonomy_cpp(data$scientificName, data$scientificNameAuthorship, col_data, threads))

  # add cleaning attributes
  attributes_to_copy <- input_attributes[!names(input_attributes) %in% c("names", "row.names")]
  attributes(data) <- c(attributes(data)[names(attributes(data)) %in% c("names", "row.names")], attributes_to_copy)
  attr(data, "clean_taxonomy") <- TRUE

  # return output
  return(data)
}


# helpers
select_kingdom <- function(col_data, kingdom){
  kingdom_ids <- col_data[col_data$`dwc:scientificName`==kingdom,]$`dwc:taxonID` # kingdom
  kingdom_ids <- c(kingdom_ids, col_data[col_data$`dwc:parentNameUsageID`==kingdom_ids,]$`dwc:taxonID`) # all phyla within kingdom
  kingdom_ids <- c(kingdom_ids, col_data[col_data$`dwc:parentNameUsageID`%in%kingdom_ids,]$`dwc:taxonID`) # class
  kingdom_ids <- c(kingdom_ids, col_data[col_data$`dwc:parentNameUsageID`%in%kingdom_ids,]$`dwc:taxonID`) # order
  kingdom_ids <- c(kingdom_ids, col_data[col_data$`dwc:parentNameUsageID`%in%kingdom_ids,]$`dwc:taxonID`) # family
  kingdom_ids <- c(kingdom_ids, col_data[col_data$`dwc:parentNameUsageID`%in%kingdom_ids,]$`dwc:taxonID`) # genus
  kingdom_ids <- c(kingdom_ids, col_data[col_data$`dwc:parentNameUsageID`%in%kingdom_ids,]$`dwc:taxonID`) # species
  kingdom_ids <- c(kingdom_ids, col_data[col_data$`dwc:parentNameUsageID`%in%kingdom_ids,]$`dwc:taxonID`) # subspecies
  kingdom_ids <- c(kingdom_ids, col_data[col_data$`dwc:acceptedNameUsageID`%in%kingdom_ids,]$`dwc:taxonID`) # synonyms
  return(col_data[col_data$`dwc:taxonID`%in%kingdom_ids,])
}

assign_tax_hier <- function(col_data){
  subspecies <- col_data[!col_data$`dwc:taxonRank`%in%c("species", "genus",
                                                        "family", "order", "class",
                                                        "phylum", "kingdom")&col_data$`dwc:taxonomicStatus`=="accepted",]
  colnames(subspecies)[colnames(subspecies)=="dwc:scientificName"] <- "subspecies"
  colnames(subspecies)[colnames(subspecies)=="dwc:parentNameUsageID"] <- "species_id"
  species <- col_data[col_data$`dwc:taxonRank`=="species"&col_data$`dwc:taxonomicStatus`=="accepted",]
  colnames(species)[colnames(species)=="dwc:scientificName"] <- "species"
  colnames(species)[colnames(species)=="dwc:parentNameUsageID"] <- "genus_id"
  genera <- col_data[col_data$`dwc:taxonRank`=="genus"&col_data$`dwc:taxonomicStatus`=="accepted",]
  colnames(genera)[colnames(genera)=="dwc:scientificName"] <- "genus"
  colnames(genera)[colnames(genera)=="dwc:parentNameUsageID"] <- "family_id"
  families <- col_data[col_data$`dwc:taxonRank`=="family"&col_data$`dwc:taxonomicStatus`=="accepted",]
  colnames(families)[colnames(families)=="dwc:scientificName"] <- "family"
  colnames(families)[colnames(families)=="dwc:parentNameUsageID"] <- "order_id"
  orders <- col_data[col_data$`dwc:taxonRank`=="order"&col_data$`dwc:taxonomicStatus`=="accepted",]
  colnames(orders)[colnames(orders)=="dwc:scientificName"] <- "order"
  colnames(orders)[colnames(orders)=="dwc:parentNameUsageID"] <- "class_id"
  classes <- col_data[col_data$`dwc:taxonRank`=="class"&col_data$`dwc:taxonomicStatus`=="accepted",]
  colnames(classes)[colnames(classes)=="dwc:scientificName"] <- "class"
  colnames(classes)[colnames(classes)=="dwc:parentNameUsageID"] <- "phylum_id"
  phyla <- col_data[col_data$`dwc:taxonRank`=="phylum"&col_data$`dwc:taxonomicStatus`=="accepted",]
  colnames(phyla)[colnames(phyla)=="dwc:scientificName"] <- "phylum"
  colnames(phyla)[colnames(phyla)=="dwc:parentNameUsageID"] <- "kingdom_id"
  kingdom <- col_data[col_data$`dwc:taxonRank`=="kingdom"&col_data$`dwc:taxonomicStatus`=="accepted",]
  colnames(kingdom)[colnames(kingdom)=="dwc:scientificName"] <- "kingdom"

  # for accepted taxa
  phyla_hier <- dplyr::left_join(phyla, kingdom[,c("dwc:taxonID", "kingdom")],
                                 by=dplyr::join_by("kingdom_id" == "dwc:taxonID"))
  class_hier <- dplyr::left_join(classes, phyla_hier[,c("dwc:taxonID", "kingdom", "phylum", "kingdom_id")],
                                 by=dplyr::join_by("phylum_id" == "dwc:taxonID"))
  order_hier <- dplyr::left_join(orders, class_hier[,c("dwc:taxonID", "kingdom", "phylum", "kingdom_id", "class", "phylum_id")],
                                 by=dplyr::join_by("class_id" == "dwc:taxonID"))
  family_hier <- dplyr::left_join(families, order_hier[,c("dwc:taxonID", "kingdom", "phylum", "kingdom_id", "class", "phylum_id", "order", "class_id")],
                                  by=dplyr::join_by("order_id" == "dwc:taxonID"))
  genus_hier <- dplyr::left_join(genera, family_hier[,c("dwc:taxonID", "kingdom", "phylum", "kingdom_id", "class", "phylum_id", "order", "class_id", "family", "order_id")],
                                 by=dplyr::join_by("family_id" == "dwc:taxonID"))
  species_hier <- dplyr::left_join(species, genus_hier[,c("dwc:taxonID", "kingdom", "phylum", "kingdom_id", "class", "phylum_id", "order", "class_id", "family", "order_id", "genus", "family_id")],
                                   by=dplyr::join_by("genus_id" == "dwc:taxonID"))
  subsp_hier <- dplyr::left_join(subspecies, species_hier[,c("dwc:taxonID", "kingdom", "phylum", "kingdom_id", "class", "phylum_id", "order", "class_id", "family", "order_id", "genus", "family_id", "species", "genus_id")],
                                 by=dplyr::join_by("species_id" == "dwc:taxonID"))


  accepted_taxa <- data.table::rbindlist(list(phyla_hier, class_hier, order_hier, family_hier,
                                              genus_hier, species_hier, subsp_hier), fill = T)

  out <- dplyr::left_join(col_data, accepted_taxa[, c("dwc:taxonID", "kingdom", "phylum",
                                                      "class", "order", "family", "genus",
                                                      "species")],
                          by=dplyr::join_by("dwc:taxonID" == "dwc:taxonID"))
  return(out)
}
