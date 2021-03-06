% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mycoportal_tab.R
\name{mycoportal_tab}
\alias{mycoportal_tab}
\title{Retrieve MyCoPortal datasets}
\usage{
mycoportal_tab(
  download_dir,
  taxon,
  country = NULL,
  state = NULL,
  county = NULL,
  locality = NULL,
  elevation_from = NULL,
  elevation_to = NULL,
  taxon_type = "1",
  north_lat = NULL,
  south_lat = NULL,
  west_lon = NULL,
  east_lon = NULL,
  point_lat = NULL,
  point_lon = NULL,
  radius = NULL,
  collector = NULL,
  collector_num = NULL,
  coll_date1 = NULL,
  coll_date2 = NULL,
  synonyms = TRUE,
  messages = TRUE,
  rec_numb = TRUE,
  read_files = TRUE
)
}
\arguments{
\item{download_dir}{Character string specifying the path to the directory where you would like files downloaded from MyCoPortal to be stored.}

\item{taxon}{Character string specifying the taxon name (e.g., species name, family name or higher taxon).}

\item{country}{Character string specifying country, e.g., "USA"}

\item{state}{Character string specifying state, e.g., "Massachusetts"}

\item{county}{Character string specifying county, e.g., "Worcester"}

\item{locality}{Character string specifying locality, e.g., "Harvard Forest"}

\item{elevation_from}{Character string; meters, e.g., "1000"}

\item{elevation_to}{Character string; meters}

\item{taxon_type}{Character, "1" to "5"; (1)Family or Scientific Name, (2)Family Only, (3)Scientific Name only, (4)Higher Taxonomy, (5)Common Name}

\item{north_lat}{Character string, bounding box coordinate; ("-90" to "90")}

\item{south_lat}{Character string, bounding box coordinate; ("-90" to "90")}

\item{west_lon}{Character string, bounding box coordinate; ("-180" to "180")}

\item{east_lon}{Character string, bounding box coordinate; ("-180" to "180")}

\item{point_lat}{Character string, point-radius coordinate; ("-90" to "90")}

\item{point_lon}{Character string, poitn-radius coordinate; ("-180" to "180")}

\item{radius}{Character string, point radius; km, e.g., "50"}

\item{collector}{Character string specifying collector's last name}

\item{collector_num}{Character string specifying collector's number}

\item{coll_date1}{Character string specifying collection data from, e.g., "19 August 1926"}

\item{coll_date2}{Character string specifying collection data from, e.g., "19 August 2018"}

\item{synonyms}{Logical. If TRUE, synonyms from MycoBank and IndexFungorum are searched}

\item{messages}{Logical. If TRUE, updates are printed when navigating to the MyCoPortal website and downloading datasets.}

\item{rec_numb}{Logical. If TRUE, the number of available records is displayed and user is prompted with yes/no question about proceeding with the download. If FASLE, number of records is not displayed and download proceeds automatically.}

\item{read_files}{Logical. If TRUE, files are downloaded into the specified directory AND are automatically imported into R as a data.frame. If FALSE, files are still downloaded into the specified directory, but not imported into R.}
}
\value{
If read_files is TRUE, a data.frame of MyCoPortal records is returned.
}
\description{
Enables programmatic interaction with the MyCoPortal web interface to retrieve complete
datasets of sporocarp records from fungaria collections and citizen-science observations.
Datasets are downloaded as tab-delimited files to a specified directory and then automatically read into R as a data.frame, if desired.
This function was modified from the \code{mycoportal} function in the rMyCoPortal package (Krah et al. 2019).
See Simpson and Schilling (2021).
}
\details{
Docker software must be installed and running on your system before using this function.
For additional details on Docker see 'Introduction' vignette via \code{vignette("help", package = "fungarium")}.\cr
\cr
All collections within MyCoPortal are queried, and selecting specific collections is
not currently supported within the function. However, collection information is listed
in the downloaded dataset, so records can be filtered by collection post-download.
}
\note{
Queries that yield a large number of records may require excessive processing
time to download and import. Use \code{rec_numb} parameter if you would like
to check for the number of records before proceeding with download (this is the default).
Exceptionally large datasets may cause memory (RAM) issues during import into R. This
depends on each system and the available memory, but when memory limits are reached
during import the resulting data.frame may be truncated; however, the MyCoPortal file will still
be stored in the specified download directory. To avoid truncation, try increasing memory
availability prior to using \code{mycoportal_tab} or set the \code{read_file} option
to FALSE and import the file into R manually (via \code{\link{read.delim}}) at a later time when
more memory may be available.
}
\examples{
library(fungarium)
#Query for all Pleurotus records in Minnesota, USA.
mp_query <- mycoportal_tab(path.expand("~"), "Pleurotus",
                           taxon_type="1", country="United States",
                           state="Minnesota", read_files=TRUE,
                           messages=FALSE, rec_numb=FALSE)
#Query for all Polyporales records in Minnesota, USA.
mp_query <- mycoportal_tab(path.expand("~"), "Polyporales",
                           taxon_type="4", country="United States",
                           state="Minnesota", read_files=TRUE,
                           messages=FALSE, rec_numb=FALSE)

}
\references{
\enumerate{
\item Krah FS, Bates S, Miller A. 2019. rMyCoPortal - an R package to interface
with the Mycology Collections Portal. Biodiversity Data Journal 7:e31511.
\item Simpson, H.J., Schilling, J.S. 2021. Using aggregated field collections data
and the novel R package fungarium to investigate fungal fire association. \emph{Mycologia}. \bold{IN PRESS}
}
}
