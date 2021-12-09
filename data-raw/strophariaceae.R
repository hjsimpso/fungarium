## code to prepare `strophariaceae` dataset goes here
strophariaceae <- data.table::fread("D:/git_repos/fungarium/data-raw/MP_strophariaceae_US_12_8_2021.tab",
                                    colClasses = "character", encoding = "UTF-8", quote = "", data.table = F)
strophariaceae <- strophariaceae[sample(1:nrow(strophariaceae), 10000), c("id","institutionCode",
                                                                "scientificName",
                                                                "scientificNameAuthorship",
                                                                "recordedBy",
                                                                "year", "eventDate",
                                                                "occurrenceRemarks", "habitat",
                                                                "associatedTaxa", "country",
                                                                "stateProvince", "county",
                                                                "decimalLatitude", "decimalLongitude",
                                                                "references")]

usethis::use_data(strophariaceae, overwrite = TRUE)

#update taxon names
strophariaceae_updated <- fungarium::taxon_update(strophariaceae, show_status=TRUE)
strophariaceae_updated <- strophariaceae_updated[strophariaceae_updated$error=="",]
strophariaceae_updated <- strophariaceae_updated[strophariaceae_updated$new_family=="Strophariaceae",]
usethis::use_data(strophariaceae_updated, overwrite = TRUE)


#assign funguild info
strophariaceae_fg <- fungarium::fg_assign(strophariaceae_updated)

#get fire enrichment per species
strophariaceae_fg <- strophariaceae_fg[strophariaceae_fg$occurrenceRemarks!=""|strophariaceae_fg$habitat!=""|strophariaceae_fg$associatedTaxa!="",]
string1 <- "(?i)charred|burn(t|ed)|scorched|fire.?(killed|damaged|scarred)|killed.by.fire"
string2 <- "(?i)un.?burn(t|ed)"
fire_records <- fungarium::find_trait(strophariaceae_fg,
                         pos_string=string1, neg_string=string2)

strophariaceae_enrich <- fungarium::enrichment(all_rec=strophariaceae_fg, trait_rec=fire_records, status_feed=T, coll_bias = T)
usethis::use_data(strophariaceae_enrich, overwrite = TRUE)
