#' Assign FUNGuild data
#'
#' Assigns FUNGuild data (e.g. trophic mode, functional guild, etc) to taxa listed in a
#' dataframe using the FUNGuild database (PLEASE CITE:Nguyen et al. 2016).
#'
#' @param tax_table  Data.frame of taxonomic classifications (e.g. phylum, class, order, family, genus, and species names)
#' @param url        URL where FUNGuild database lives. Current default: http://www.stbates.org/funguild_db.php.
#' @param tax_cols   Character vector specifying the column names containing taxonomic variables. Names must be in descending order of taxonomic rank.
#' Default is c("new_phylum", "new_class", "new_order", "new_family", "new_genus", "new_species").
#'
#' @return           Returns the input data.frame with FUNGuild assignments appended.
#' @references
#' \enumerate{
#'   \item Nguyen NH, Song Z, Bates ST, Branco S, Tedersoo L, Menke J, Schilling JS, Kennedy PG. 2016. FUNGuild: An open annotation tool for parsing fungal community datasets by ecological guild. \emph{Fungal Ecology}, 20: 241-248. doi:10.1016/j.funeco.2015.06.006
#' }
#' @export
#'
#' @examples
#' library(fungarium)
#' data(agaricales_updated) #import sample dataset with updated taxon names
#'
#' #get funguild assignments
#' agaricales_fg <- fg_assign(agaricales_updated)
#'
fg_assign <- function(tax_table,
                      url = "http://www.stbates.org/funguild_db.php",
                      tax_cols = c("new_phylum", "new_class", "new_order", "new_family", "new_genus", "new_species")){
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
  #check input data format
  if (!is.data.frame(tax_table)){
    stop("Input data must be a data.frame.")
  }

  #make unique
  tax_table_u <- dplyr::distinct(tax_table[,tax_cols])

  #download FUNGuild database
  fg <- xml2::read_html(url)
  fg <- rvest::html_text(fg)
  fg <- jsonlite::fromJSON(gsub("funguild_db_2", "", fg))

  #add columns to tax table for fg output.
  out <- data.frame(matrix(nrow=nrow(tax_table_u),ncol = 7))
  colnames(out) <- colnames(fg)[4:10]
  tax_table_u <- cbind(tax_table_u,out)

  #matching taxa in input data with taxa in FUNGuild database
  for(i in 1:nrow(tax_table_u)){
    j <- length(tax_cols)
    assigned <- F
    while (assigned==F & j>0){
      if (tax_table_u[[tax_cols[j]]][i] %in% fg$taxon){
        assigned <- T
        tax_table_u[i,(ncol(tax_table_u) - 6):ncol(tax_table_u)] <- fg[match(tax_table_u[[tax_cols[j]]][i],fg$taxon),4:10]
      }
      j <- j-1
    }
  }

  #report and return output
  cat(sum(!is.na(tax_table_u$guild))/(nrow(tax_table_u))*100,'% of taxa assigned a functional guild.', sep = '')
  for (i in 1:length(tax_cols)){
    if(i==1){
      tax_table$comb <- tax_table[,tax_cols[i]]
    }else{
      tax_table$comb <- paste0(tax_table$comb, tax_table[,tax_cols[i]])
    }
  }
  for (i in 1:length(tax_cols)){
    if(i==1){
      tax_table_u$comb <- tax_table_u[,tax_cols[i]]
    }else{
      tax_table_u$comb <- paste0(tax_table_u$comb, tax_table_u[,tax_cols[i]])
    }
  }
  tax_table <- dplyr::inner_join(tax_table, tax_table_u[,utils::tail(colnames(tax_table_u),8)], by="comb")
  tax_table <- tax_table[,!colnames(tax_table)%in%"comb"]
  return(tax_table)
}
