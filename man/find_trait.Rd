% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_trait.R
\name{find_trait}
\alias{find_trait}
\title{Find fungal records with specific trait data}
\usage{
find_trait(
  data,
  metadata_cols,
  pos_string,
  neg_string = NULL,
  string_clean = TRUE
)
}
\arguments{
\item{data}{Data.frame containing columns of trait-relevant metadata.}

\item{metadata_cols}{Character vector containing names of columns with trait-relevant metadata.}

\item{pos_string}{Character string ("positive") containing a \link{regular expression} that is used to find character strings within the specified metadata columns of the input data frame that contain trait-relevant keywords or phrases.}

\item{neg_string}{Character string ("negative") containing a regular expression that is used to remove records that were falsely identified, via the "positive" search string, as being trait-relevant. This argument is optional.}

\item{string_clean}{Logical. If TRUE (the default), strings in metadata_cols are "cleaned" prior to trait searching. This includes converting strings to lowercase and removing any punctuation (e.g., periods, commas, question marks, etc.) or extra white space.}
}
\value{
\preformatted{      Returns a data.frame of records with trait-relevant metadata.
}
}
\description{
Find records within a dataset of fungal collections/observations
(e.g. \href{https://mycoportal.org}{MyCoPortal} dataset)
that have environmental metadata relevant to a particular trait.
Example traits: growing on wood, soil, or leaves;
growing on downed log or standing tree;
growing on or near a specific plant taxon;
growing on or near other fungal taxa;
growing in a habitat affected by a particular disturbance (e.g. wind, fire, logging)
}
\note{
Current trait-relevant fields (i.e., \code{metadata_cols}) in MyCoPortal data include "occurrenceRemarks",
"habitat", and "substrate". The "host" field, which contained trait-relevant info, was present in older MyCoPortal datasets, but has been removed as of April 2021.
It is not yet clear where this data was moved to. It's possible that it was moved to the "associatedTaxa" field. This field already existed in MyCoPortal datasets but was typically blank.
Either way, it appears that "associatedTaxa" may be a useful field to use for \code{metadata_cols}; however, this may depend on the trait of interest, so use your best judgment.
}
\examples{
library(fungarium)
data(strophariaceae) #import sample dataset
data <- strophariaceae

#Finds fire-associated records
string1 <- "(?i)charred|burn(t|ed)|scorched|fire.?(killed|damaged|scarred)|killed.by.fire"

#Removes records falsely identified as fire-associated
string2 <- "(?i)un.?burn(t|ed)"

#filter out records that do not contain any environmental metadata (optional)
#note that "host" is no longer present in new MyCoPortal data sets
data <- data[data$occurrenceRemarks!=""|data$host!=""|
                   data$habitat!=""|data$substrate!="",]

#find trait-relevant records
trait_data <- find_trait(data, c("occurrenceRemarks", "host", "habitat", "substrate"),
                         pos_string=string1, neg_string=string2)
}
