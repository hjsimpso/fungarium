#' View list of collections in MyCoPortal
#'
#' Displays the names of all collections available through the MyCoPortal, along
#' with the associated abbreviation, numerical code, and order in which they appear on
#' the MyCoPortal website.
#'
#' @return
#' Data.frame containing the following:
#' \item{coll_name}{name of collection}
#' \item{coll_abbrev}{abbreviated collection name}
#' \item{coll_code}{numerical collection code}
#' \item{coll_order}{order in which the collections appear on the MyCoPortal website}
#'
#' @details
#' Collection codes can be used within \code{mycoportal_tab} to retrieve MyCoPortal
#' data from specific collections only.
#'
#' @export
#' @examples
#' library(fungarium)
#' mp_collections()
#'
mp_collections <- function(){
  link <- suppressWarnings(readLines("https://mycoportal.org/portal/collections/index.php"))
  colls <- grep("collectionname", link, value=T)
  colls_df <- data.frame(coll_name=gsub(".*class=\"collectionname\">(.+)</div> <div class=\"collectioncode\">.*", "\\1", colls))
  colls_df$coll_abbrev <- gsub(".*<div class=\"collectioncode\">\\((.+)\\)</div>.*", "\\1", colls)
  colls_df$coll_code <- gsub(".*collid=([0-9]+).*", "\\1", colls)
  colls_df <- dplyr::distinct(colls_df)
  colls_df$order <- 1:nrow(colls_df)
  return(colls_df)

}
