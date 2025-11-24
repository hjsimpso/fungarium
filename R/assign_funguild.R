#' @title Assign FUNGuild data
#'
#' @description
#' Assigns FUNGuild data (e.g. trophic mode, functional guild, etc) to taxa listed in a
#' dataframe using the FUNGuild database (PLEASE CITE:Nguyen et al. 2016).
#'
#' @param data        `Data.frame`.
#' @param url         Character. URL where FUNGuild database lives. Current default: http://www.stbates.org/funguild_db.php.
#' @param tax_cols    Character. Column names containing taxonomic variables. Names must be in descending order of taxonomic rank. Default is c("phylum_pres", "class_pres", "order_pres", "family_pres", "genus_pres", "species_pres").
#'
#' @return           Input `data.frame` with FUNGuild assignments appended.
#' @references
#' \enumerate{
#'   \item Nguyen NH, Song Z, Bates ST, Branco S, Tedersoo L, Menke J, Schilling JS, Kennedy PG. 2016. FUNGuild: An open annotation tool for parsing fungal community datasets by ecological guild. \emph{Fungal Ecology}, 20: 241-248. doi:10.1016/j.funeco.2015.06.006
#' }
#' @export
#'
#' @examples
#' library(fungarium)
#' data(agaricales) #import sample data set
#' clean_tax <- clean_taxonomy(as_dwca(agaricales)) #clean taxonomy
#' funguild_data <- assign_funguild(clean_tax) #get funguild assignments
#'
assign_funguild <- function(data,
                            url = "http://www.stbates.org/funguild_db.php",
                            tax_cols = c("phylum_pres", "class_pres", "order_pres", "family_pres", "genus_pres", "species_pres")){
  #check for dependencies
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop("Please install the \"rvest\" package.",
         call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install the \"jsonlite\" package",
         call. = FALSE)
  }
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Please install the \"xml2\" package",
         call. = FALSE)
  }

  # check args
  checkmate::assert_data_frame(data)
  checkmate::assertCharacter(url, max.len = 1)
  checkmate::assertCharacter(tax_cols)
  lapply(tax_cols, checkmate::assert_choice, choices=colnames(data), .var.name='tax_cols')

  checkmate::assert_true(attr(data, "clean_taxonomy")) # TODO better error reporting

  # get attributes
  input_attributes <- attributes(data)

  #make unique
  data_u <- dplyr::distinct(data[,tax_cols])

  #download FUNGuild database
  fg <- xml2::read_html(url)
  fg <- rvest::html_text(fg)
  fg <- jsonlite::fromJSON(gsub("funguild_db", "", fg))

  #add columns to tax table for fg output.
  out <- data.frame(matrix(nrow=nrow(data_u),ncol = 7))
  colnames(out) <- colnames(fg)[4:10]
  data_u <- cbind(data_u,out)

  #matching taxa in input data with taxa in FUNGuild database
  for(i in 1:nrow(data_u)){
    j <- length(tax_cols)
    assigned <- F
    while (assigned==F & j>0){
      if (data_u[[tax_cols[j]]][i] %in% fg$taxon){
        assigned <- T
        data_u[i,(ncol(data_u) - 6):ncol(data_u)] <- fg[match(data_u[[tax_cols[j]]][i],fg$taxon),4:10]
      }
      j <- j-1
    }
  }

  #report and return output
  cat(sum(!is.na(data_u$guild))/(nrow(data_u))*100,'% of taxa assigned a functional guild.', sep = '')
  for (i in 1:length(tax_cols)){
    if(i==1){
      data$comb <- data[,tax_cols[i]]
    }else{
      data$comb <- paste0(data$comb, data[,tax_cols[i]])
    }
  }
  for (i in 1:length(tax_cols)){
    if(i==1){
      data_u$comb <- data_u[,tax_cols[i]]
    }else{
      data_u$comb <- paste0(data_u$comb, data_u[,tax_cols[i]])
    }
  }
  data <- dplyr::inner_join(data, data_u[,utils::tail(colnames(data_u),8)], by="comb")
  data <- data[,!colnames(data)%in%"comb"]

  # add attributes
  attributes_to_copy <- input_attributes[!names(input_attributes) %in% c("names", "row.names")]
  attributes(data) <- c(attributes(data)[names(attributes(data)) %in% c("names", "row.names")], attributes_to_copy)

  # output
  return(data)
}
