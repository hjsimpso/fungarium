#' @title Clean and harmonize species names
#'
#' @description
#' Parses species names...
#'
#' @param data `dwca` object.
#' @param kingdom Character. `Fungi` or `Plantae`. Used to speed up taxon matching. Default: `Fungi`.
#' @param refresh_db Logical. Should the COL database be refreshed. Default: `FASLE`. Note, database is always downloaded during the first execution of this function after package installation.
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
#' clean_dates <- clean_date(as_dwca(agaricales)) #clean dates
#'

clean_taxonomy <- function(data, kingdom = "Fungi", refresh_db = F,
                           db_url = "https://download.catalogueoflife.org/col/annual/2024_dwca.zip"){
  # check args
  if (!inherits(data, "dwca")) {
    stop("'data' must be of class 'dwca'. Use `as_dwca()` first.")
  }
  checkmate::assert_character(kindom, max.len = 1)
  checkmate::assert_choice(kingdom, c("Fungi", "Plantae"))
  checkmate::assert_logical(refresh_db, max.len = 1)
  checkmate::assert_character(db_url, max.len = 1)


  # download fresh COL data
  if (!file.exists("col_dwca/Taxon.tsv")||refresh_db){
    print("Downloading COL taxonomy db...")

    # URL for the latest COL Darwin Core Archive
    # url <- "https://api.checklistbank.org/dataset/310958/export.zip?extended=true&format=DwCA"

    # Download the zip file
    download.file(db_url, destfile = "col_dwca.zip", mode = "wb")
    unzip("col_dwca.zip", exdir = "col_dwca")

    # load and clean
    col_data <- data.table::fread(col_data_file, sep="\t", quote="", data.table = F)
    fungi <- select_kingdom(col_data, "Fungi")
    fungi <- assign_tax_hier(fungi)
    data.table::fwrite(fungi, "col_dwca/Taxon_fungi_w_tax_hier.tsv", sep = '\t', quote = "")

    plants <- select_kingdom(col_data, "Plantae")
    plants <- assign_tax_hier(plants)
    data.table::fwrite(plants, "col_dwca/Taxon_plantae_w_tax_hier.tsv", sep = '\t', quote = "")

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
      col_data_file <- "col_dwca/Taxon_fungi_w_tax_hier.tsv"
    } else{
      col_data_file <- "col_dwca/Taxon_plantae_w_tax_hier.tsv"
    }
    col_data <- data.table::fread(col_data_file,sep="\t", quote="", data.table = F)
  }


  # Call cpp function
  data <- cbind(data, clean_taxonomy_cpp(data$scientificName, data$scientificNameAuthorship, col_data))

  # return dwca class object
  class(data) <- c("dwca", "data.frame")
  data
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
  kingdom_ids <- c(kingdom_ids, col_data[col_data$`dwc:acceptedNameUsageID`%in%kingdom_ids,]$`dwc:taxonID`) # synonyms
  return(col_data[col_data$`dwc:taxonID`%in%kingdom_ids,])
}

assign_tax_hier <- function(col_data){

  genera <- col_data[col_data$`dwc:taxonRank`=="genus",]
  colnames(genera)[colnames(genera)=="dwc:scientificName"] <- "genus"
  colnames(genera)[colnames(genera)=="dwc:parentNameUsageID"] <- "family_id"
  families <- col_data[col_data$`dwc:taxonRank`=="family",]
  colnames(families)[colnames(families)=="dwc:scientificName"] <- "family"
  colnames(families)[colnames(families)=="dwc:parentNameUsageID"] <- "order_id"
  orders <- col_data[col_data$`dwc:taxonRank`=="order",]
  colnames(orders)[colnames(orders)=="dwc:scientificName"] <- "order"
  colnames(orders)[colnames(orders)=="dwc:parentNameUsageID"] <- "class_id"
  classes <- col_data[col_data$`dwc:taxonRank`=="class",]
  colnames(classes)[colnames(classes)=="dwc:scientificName"] <- "class"
  colnames(classes)[colnames(classes)=="dwc:parentNameUsageID"] <- "phylum_id"
  phyla <- col_data[col_data$`dwc:taxonRank`=="phylum",]
  colnames(phyla)[colnames(phyla)=="dwc:scientificName"] <- "phylum"
  colnames(phyla)[colnames(phyla)=="dwc:parentNameUsageID"] <- "kingdom_id"
  kingdom <- col_data[col_data$`dwc:taxonRank`=="kingdom",]
  colnames(kingdom)[colnames(kingdom)=="dwc:scientificName"] <- "kingdom"

  tax_hier <- dplyr::left_join(phyla, kingdom[,c("dwc:taxonID", "kingdom")],
                               by=dplyr::join_by("kingdom_id" == "dwc:taxonID"))
  tax_hier <- dplyr::left_join(classes, phyla[,c("dwc:taxonID", "phylum", "kingdom_id")],
                               by=dplyr::join_by("phylum_id" == "dwc:taxonID"))
  tax_hier <- dplyr::left_join(orders, tax_hier[,c("dwc:taxonID", "phylum", "kingdom_id", "class", "phylum_id")],
                               by=dplyr::join_by("class_id" == "dwc:taxonID"))
  tax_hier <- dplyr::left_join(families, tax_hier[,c("dwc:taxonID", "phylum", "kingdom_id", "class", "phylum_id", "order", "class_id")],
                               by=dplyr::join_by("order_id" == "dwc:taxonID"))
  tax_hier <- dplyr::left_join(genera, tax_hier[,c("dwc:taxonID", "phylum", "kingdom_id", "class", "phylum_id", "order", "class_id", "family", "order_id")],
                               by=dplyr::join_by("family_id" == "dwc:taxonID"))
  tax_hier <- dplyr::left_join(col_data, tax_hier[,c("dwc:taxonID", "phylum", "kingdom_id", "class", "phylum_id", "order", "class_id", "family", "order_id", "genus", "family_id")],
                               by=dplyr::join_by("dwc:parentNameUsageID" == "dwc:taxonID"))

  return(tax_hier)
}
