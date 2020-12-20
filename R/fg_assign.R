#' Assign FUNGuild data
#'
#' Assigns FUNGuild data (e.g. trophic mode, functional guild, etc) to taxa listed in a
#' dataframe using the FUNGuild database (PLEASE CITE:Nguyen et al. 2016).
#' This is a modifed version of the fg_assign function originally built by
#' Colin Averill (https://github.com/colinaverill/fg_assign.r).
#'
#' @param tax_table  Dataframe of taxonomic classifications. Must include columns with phylum, class, order, family, genus, and species names. Missing values is allowed, but may inhibit the assignment of FUNGuild data to that row.
#' @param url        URL where FUNGuild database lives. Current default: http://www.stbates.org/funguild_db.php.
#' @param phylum_col     Character string specifying the phylum column name. Default is "new_phylum" (name given in the output of \code{\link[fungarium]{taxon_update}}).
#' @param class_col      Character string specifying the class column name. Default is "new_class".
#' @param order_col      Character string specifying the order column name. Default is "new_order".
#' @param family_col     Character string specifying the family column name. Default is "new_family".
#' @param genus_col      Character string specifying the genus column name. Default is "new_genus".
#' @param species_col    Character string specifying the species column name (i.e. full canonical name, not just specific epithet). Default is "new_name".
#'
#' @return           Returns the input dataframe with FUNGuild assignments appended.
#' @references
#' \enumerate{
#'   \item Nguyen NH, Song Z, Bates ST, Branco S, Tedersoo L, Menke J, Schilling JS, Kennedy PG. 2016. FUNGuild: An open annotation tool for parsing fungal community datasets by ecological guild. \emph{Fungal Ecology}, 20: 241-248. doi:10.1016/j.funeco.2015.06.006
#'   \item Simpson, H.J., Schilling, J.S. 2020. Using aggregated field collections data and the novel R package “fungarium” to investigate fungal fire association. \emph{Mycologia}.
#' }
#' @export
#'
#' @examples
#'
#' mycoportal_data <- taxon_update(mycoportal_data)
#' guild_data <- fg_assign(mycoportal_data)
#'
fg_assign <- function(tax_table, url = "http://www.stbates.org/funguild_db.php", phylum_col="new_phylum", class_col="new_class", order_col="new_order", family_col="new_family", genus_col="new_genus", species_col="new_name"){
  #check for dependencies
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop("Please install the \"rvest\" package.",
         call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install the \"jsonlite\" package",
         call. = FALSE)
  }

  #check that the input is formatted right. If not, stop, throw an error.
  if (!is.data.frame(tax_table)){
    stop('Taxonomy input data needs to be a data.frame.')
  }

  #download FUNGuild database, convert it to something R interpretable.
  fg <- xml2::read_html(url)
  fg <- rvest::html_text(fg)
  fg <- jsonlite::fromJSON(gsub("funguild_db", "", fg))

  #add columns to tax table for fg output.
  out <- data.frame(matrix(nrow=nrow(tax_table),ncol = 7))
  colnames(out) <- colnames(fg)[4:10]
  tax_table <- cbind(tax_table,out)

  #matching taxa in input dataframe with taxa in FUNGuild database.
  for(i in 1:nrow(tax_table)){#species level match
    if(tax_table[[species_col]][i] %in% fg$taxon){
      tax_table[i,(ncol(tax_table) - 6):ncol(tax_table)] <- fg[match(tax_table[[species_col]][i],fg$taxon),4:10]
    }else{
      if(tax_table[[genus_col]][i] %in% fg$taxon){#genus level match
        tax_table[i,(ncol(tax_table) - 6):ncol(tax_table)] <- fg[match(tax_table[[genus_col]][i],fg$taxon),4:10]
      }else{
        if(tax_table[[family_col]][i] %in% fg$taxon){#family level match
          tax_table[i,(ncol(tax_table) - 6):ncol(tax_table)] <- fg[match(tax_table[[family_col]][i],fg$taxon),4:10]
        }else{
          if(tax_table[[order_col]][i] %in% fg$taxon){#order level match
            tax_table[i,(ncol(tax_table) - 6):ncol(tax_table)] <- fg[match(tax_table[[order_col]][i],fg$taxon),4:10]
          }else{
            if(tax_table[[class_col]][i] %in% fg$taxon){#class level match
              tax_table[i,(ncol(tax_table) - 6):ncol(tax_table)] <- fg[match(tax_table[[class_col]][i],fg$taxon),4:10]
            }else{
              if(tax_table[[phylum_col]][i] %in% fg$taxon){#phylum level match
                tax_table[i,(ncol(tax_table) - 6):ncol(tax_table)] <- fg[match(tax_table[[phylum_col]][i],fg$taxon),4:10]
              }
            }
          }
        }
      }
    }
  }

  #report and return output.
  cat(sum(!is.na(tax_table$guild))/(nrow(tax_table))*100,'% of taxa assigned a functional guild.', sep = '')
  return(tax_table)
}
