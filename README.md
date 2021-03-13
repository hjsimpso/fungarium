
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fungarium

<!-- badges: start -->

![GitHub](https://img.shields.io/github/license/hjsimpso/fungarium)
[![GitHub
release](https://img.shields.io/github/release/hjsimpso/fungarium.svg)](https://GitHub.com/hjsimpso/fungarium/releases/)
![Github
commits](https://img.shields.io/github/commits-since/hjsimpso/fungarium/latest)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/hjsimpso/fungarium/graphs/commit-activity)

<!-- badges: end -->

An R package for enhancing the access and analysis of fungaria
collections data.

## Introduction

### Installing fungarium

``` r
install.packages("devtools") #Install 'devtools' (if not already installed)
devtools::install_github("hjsimpso/fungarium") #Retrieve fungarium package from github repository and install
```

### Installing Docker

[Docker](https://docs.docker.com/get-started/overview/) software needs
to be installed on your system in order to utilize the `mycoportal_tab`
function. All other functions in the fungarium package do not require
Docker, so if you do not wish to utilize `mycoportal_tab` you do not
need to install Docker.

Instructions for installing Docker (available for Linux platforms,
macOS, and Windows 10): <https://docs.docker.com/engine/install/>

## Retrieving MyCoPortal datasets

Manually interacting with the
[MyCoPortal](https://mycoportal.org/portal/index.php) web interface can
be a time consuming process, especially if one needs to submit a large
variety of separate queries for various taxa, locations, dates, etc. The
`mycoportal_tab` function provides a convenient alternative by allowing
users to submit queries and download complete datasets from entirely
within the R programming environment.

Examples:

``` r
# Check that Docker is installed and running on your system

# Load fungarium
library(fungarium) 

# Submit queries to MyCoPortal and retrieve complete datasets
# MyCoPortal tab-delmited file is saved in specified directory and then imported into R as a data.frame
query1 <- mycoportal_tab("/path/to/directory", "Agaricus", taxon_type = "1", country = "United States", state="Minnesota") 
query1[1:10, c("scientificName", "country", "stateProvince")] # Preview dataset

query2 <- mycoportal_tab("/path/to/directory", "Russulaceae", taxon_type = "2", country = "United States", state="Minnesota") 
query2[1:10, c("scientificName", "country", "stateProvince")] # Preview dataset

query3 <- mycoportal_tab("/path/to/directory", "Polyporales", taxon_type = "4", country = "United States", state="Minnesota") 
query3[1:10, c("scientificName", "country", "stateProvince")] # Preview dataset
```

## Updating taxon names

Taxon names in MyCoPortal datasets are often outdated and do not reflect
current scientific consensus. Some have doubtful taxonomic status or
have synonyms with accepted status. `taxon_update` updates or confirms
the validty of the names listed using taxonomic information maintained
by
[GBIF](https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c).

Example:

``` r
query3 <- mycoportal_tab("/path/to/directory", "Polyporales", taxon_type = "4", country = "United States", state="Minnesota") 
updated_names <- taxon_update(query3) # Returns input dataframe with updated names appended
```

## Fixing US location names and assigning fips codes

Location names (e.g. “stateProvince”, “county”) in MyCoPortal datasets
are often mispelled or use inconsistent nomenclature (e.g. “Anoka
County”, “Anoka Co. ”Anoka"). For United States datasets, `get_fips`
finds the best matches for mispelled states and counties and assigns
relevant fips codes. Because county names are not always unique to a
give state, fips codes serves as a unique identifier that facilitates
county-based geographic analyses.

Example:

``` r
query3 <- mycoportal_tab("/path/to/directory", "Polyporales", taxon_type = "4", country = "United States", state="Minnesota")
fips_codes <- get_fips(query3) # Returned input dataframe with fixed location names and fips codes appended
```

## Finding fungal traits

MyCoPortal records often contain environmental metadata that is relevant
to ecological traits, like habitat, host, or substrate associations.
These data, however, are not standardized and vary in structure and
detail between collectors. Searching for records that exhibit a certain
trait can therefore be challenging. `find_trait` facilitates this
searching process by using complex regex search strings to search for a
trait in a variety of input metadata fields.

``` r
query3 <- mycoportal_tab("/path/to/directory", "Polyporales", taxon_type = "4", country = "United States", state="Minnesota")

#Finds records that are relevant to fire association (i.e. associated with fire-affected habitats, hosts, or substrates)
string1 <- "(?i)charred|(?i)burn(t|ed)|(?i)scorched|(?i)fire.?(killed|damaged|scarred)|(?i)killed.by.fire"

trait_records <- find_trait(query3, string1) #Returns a dataframe of records exhibiting the trait of interest
```

## Assign FunGuild data

[FunGuild](http://www.funguild.org/) data (e.g. trophic mode, functional
guild) may be useful in interpretting different patterns observed within
MyCoPortal datasets. `fg_assign` assigns FunGuild data to MyCoPortal
records to facilitate these analyses.

``` r
query3 <- mycoportal_tab("/path/to/directory", "Polyporales", taxon_type = "4", country = "United States", state="Minnesota")
query3_updated <- taxon_update(query3) # Update taxon names before assigning FunGuild data
guild_data <- fg_assign(query3_updated) # Returns the input dataframe with FunGuild data appended
```

## Example trait analysis: fungal fire association

See full
[example](https://github.com/hjsimpso/fungarium/blob/main/Example_fire_association.md)
of how `fungarium` was used to help analyze taxonomic, geographic, and
temporal patterns of fungal fire association.

## Meta

  - Get citation information for fungarium in R via `citation(package =
    'fungarium')`
  - This package is released with a [Contributor Code of
    Conduct](https://github.com/hjsimpso/fungarium/blob/main/CODE_OF_CONDUCT.md).
    By contributing to this project, you agree to abide by its terms.
