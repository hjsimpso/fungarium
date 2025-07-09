aggregate_occ <- function(taxon,
                          date,
                          lat,
                          lon,
                          country,
                          state_province){
  checkmate::assert_character(taxon)
  checkmate::assert_character(country)
  checkmate::assert_character(state_province)
  checkmate::assert_date(date)
  checkmate::assert_numeric(lat)
  checkmate::assert_numeric(lon)
  # Check they have the same length
  lengths <- c(length(taxon),
               length(date),
               length(lat),
               length(lon),
               length(country),
               length(state_province))

  checkmate::assert_true(length(unique(lengths)) == 1,
              .var.name = "Arguments must all have the same length.")
  # checkmate::assert(length(taxon)==
  #                     length(date)==
  #                     length(lat)==
  #                     length(lon)==
  #                     length(country)==
  #                     length(state_province))

  print("Hello!")
}
