Installing fungarium package
----------------------------

``` r
install.packages("devtools") #Install 'devtools' (if not already installed)
devtools::install_github("hjsimpso/fungarium") #Retrieve fungarium package from github repository and install
```

Installing Docker
-----------------

Docker software
(<a href="https://docs.docker.com/get-started/overview/" class="uri">https://docs.docker.com/get-started/overview/</a>)
needs to be installed on your system in order to utilize the
`mycoportal_tab` function. All other functions in the fungarium package
do not require Docker, so if you do not wish to utilize `mycoportal_tab`
you do not need to install Docker.

Instructions for installing Docker (available for Linux platforms,
macOS, and Windows 10):
<a href="https://docs.docker.com/engine/install/" class="uri">https://docs.docker.com/engine/install/</a>

Retrieving MyCoPortal datasets
------------------------------

Manually interacting with the MyCoPortal web interface can be a time
consuming process, especially if one needs to submit a large variety of
separate queries for various taxa, locations, dates, etc. The
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
query1 <- mycoportal_tab("/path/to/directory", "Agaricus", taxon_type = "1", country = "United States", state="Minnesota", messages = F, rec_numb = F) 
query1[1:10, c("scientificName", "country", "stateProvince")] # Preview dataset

query2 <- mycoportal_tab("/path/to/directory", "Russulaceae", taxon_type = "2", country = "United States", state="Minnesota", messages = F, rec_numb = F) 
query2[1:10, c("scientificName", "country", "stateProvince")] # Preview dataset

query3 <- mycoportal_tab("/path/to/directory", "Polyporales", taxon_type = "4", country = "United States", state="Minnesota", messages = F, rec_numb = F) 
query3[1:10, c("scientificName", "country", "stateProvince")] # Preview dataset
```
