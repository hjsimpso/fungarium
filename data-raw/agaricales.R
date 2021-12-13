## code to prepare `agaricales` dataset goes here
agaricales <- data.table::fread("D:/git_repos/fungarium/data-raw/1639090593-occur.tab",
                                    colClasses = "character", encoding = "UTF-8", quote = "", data.table = F)
set.seed(123)
agaricales <- agaricales[sample(1:nrow(agaricales), 50000), c("id","institutionCode",
                                                                          "scientificName",
                                                                          "scientificNameAuthorship",
                                                                          "recordedBy",
                                                                          "year", "eventDate",
                                                                          "occurrenceRemarks", "habitat",
                                                                          "associatedTaxa", "country",
                                                                          "stateProvince", "county",
                                                                          "decimalLatitude", "decimalLongitude",
                                                                          "references")]
agaricales0 <- agaricales
agaricales <- agaricales[sample(1:nrow(agaricales), 1000),] #trim for saving sample data set within package
usethis::use_data(agaricales, overwrite = TRUE)

#update taxon names
agaricales_updated <- fungarium::taxon_update(agaricales0, show_status=TRUE)
agaricales_updated <- agaricales_updated[agaricales_updated$error=="",]
agaricales_updated <- agaricales_updated[agaricales_updated$new_order=="Agaricales",]
usethis::use_data(agaricales_updated, overwrite = TRUE)


#assign funguild info
agaricales_fg <- fungarium::fg_assign(agaricales_updated)

#get fire enrichment per species
agaricales_fg <- agaricales_fg[agaricales_fg$occurrenceRemarks!=""|agaricales_fg$habitat!=""|agaricales_fg$associatedTaxa!="",]
string1 <- "(?i)charred|burn(t|ed)|scorched|fire.?(killed|damaged|scarred)|killed.by.fire"
string2 <- "(?i)un.?burn(t|ed)"
fire_records <- fungarium::find_trait(agaricales_fg,
                                      pos_string=string1, neg_string=string2)

agaricales_enrich <- fungarium::enrichment(all_rec=agaricales_fg, trait_rec=fire_records, status_feed=T, coll_bias = T)
usethis::use_data(agaricales_enrich, overwrite = TRUE)
