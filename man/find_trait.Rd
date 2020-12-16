% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_trait.R
\name{find_trait}
\alias{find_trait}
\title{Find fungal records with specific trait data}
\usage{
find_trait(
  data,
  pos_string,
  metadata_cols = c("host", "substrate", "habitat", "occurrenceRemarks"),
  neg_string = NULL
)
}
\arguments{
\item{data}{Dataframe containing columns of trait-relevant metadata.}

\item{pos_string}{Character string ("positive") containing a \link{regular expression} that is used to find character strings within the specified metadata columns of the input data frame that contain trait-relevant keywords or phrases.}

\item{metadata_cols}{Character vector containing names of columns with trait-relevant metadata. Default names are "occurrenceRemarks", "host", "habitat", and "substrate" (names used in MyCoPortal datasets).}

\item{neg_string}{Character string ("negative") containing a regular expression that is used to remove records that were falsely identfied, via the "positive" search string, as being trait-relevant. This argument is optional.}
}
\value{
\preformatted{      Returns a dataframe of records with trait-relevant metadata.
}
}
\description{
Find records within a dataset of fungal collections/observations
(e.g. \href{https://mycoportal.org/portal/index.php}{MyCoPortal} dataset)
that have environmental metadata relevant to a particular trait.
Example traits: growing on wood, soil, or leaves;
growing on downed log or standing tree;
growing on or near a specific plant taxon;
growing on or near other fungal taxa;
growing in a habitat affected by a particular disturbance (e.g. wind, fire, logging)
}
\examples{
MP_data <- mycoportal_tab("Pleurotus")
MP_data_updated <- taxon_update(MP_data)

#Finds records that are relevant to fire association
  string1 <- "(?i)charred|(?i)burn(t|ed)|(?i)scorched|
    (?i)fire.?(killed|damaged|scarred)|(?i)killed.by.fire"

#Removes records that were falsely identfied as
being fire-associated because they contain the words "burnt" or "burned"
  string2 <- "(?i)un.?burn(t|ed)"

trait_records <- find_trait(MP_data_updated, string1,
  trait_columns, string2)
}
\references{
\enumerate{
\item Nguyen NH, Song Z, Bates ST, Branco S, Tedersoo L, Menke J, Schilling JS, Kennedy PG. 2016. FUNGuild: An open annotation tool for parsing fungal community datasets by ecological guild. \emph{Fungal Ecology}, 20: 241-248. doi:10.1016/j.funeco.2015.06.006
\item Simpson, H.J., Schilling, J.S. 2020. Using aggregated field collections data and the novel R package “fungarium” to investigate fungal fire association. \emph{Mycologia}.
}
}