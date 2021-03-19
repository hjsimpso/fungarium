## code to prepare `strophariaceae` dataset goes here
strophariaceae <- data.table::fread("C:/Users/hjs92/Documents/git_repos/fungarium/data-raw/strophariaceae.tab",
                                    colClasses = "character", encoding = "Latin-1", quote = "")
data <- strophariaceae[sample(1:nrow(strophariaceae), 10000), c("id","institutionCode",
                                                                "scientificName",
                                                                "scientificNameAuthorship",
                                                                "recordedBy",
                                                                "associatedCollectors",
                                                                "year", "eventDate",
                                                                "occurrenceRemarks", "habitat",
                                                                "substrate", "host", "country",
                                                                "stateProvince", "county",
                                                                "decimalLatitude", "decimalLongitude",
                                                                "references")]

usethis::use_data(strophariaceae, overwrite = TRUE)
