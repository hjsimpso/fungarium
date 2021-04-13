## code to prepare `agaricales` dataset goes here
data <- data.table::fread("C:/Users/hjs92/Documents/git_repos/fungarium/data-raw/global_pezizomycotina_agaricomycotina_fire_ratio_3_4_2021.tab",
                                    colClasses = "character", encoding = "Latin-1", quote = "")
agaricales_enrich <- data[data$new_order=="Agaricales",]
colnames(agaricales_enrich)[colnames(agaricales_enrich)=="fire_freq"] <- "trait_freq"
colnames(agaricales_enrich)[colnames(agaricales_enrich)=="fire_ratio"] <- "trait_ratio"
agaricales_enrich$trait_ratio <- as.numeric(agaricales_enrich$trait_ratio)
agaricales_enrich$trait_freq <- as.numeric(agaricales_enrich$trait_freq)
agaricales_enrich$freq <- as.numeric(agaricales_enrich$freq)
agaricales_enrich$max_bias <- as.numeric(agaricales_enrich$max_bias)
agaricales_enrich$coll_groups <- as.numeric(agaricales_enrich$coll_groups)
agaricales_enrich$max_bias_t <- as.numeric(agaricales_enrich$max_bias_t)
agaricales_enrich$coll_groups_t <- as.numeric(agaricales_enrich$coll_groups_t)
usethis::use_data(agaricales_enrich, overwrite = TRUE)
