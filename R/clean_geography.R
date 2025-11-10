#' @title Clean and harmonize heterogeneous geographic data (lat/lon/country)
#'
#' @description
#' Parse varying formats/spellings for geographic data into a consistent formats
#' and check the validity of lat/lon coordinates using various tests.
#'
#' @param data `dwca` object.
#' @param tests Character. Coordinate cleaning tests to perform. Options include: "zero", "equal". Default is all tests.
#'
#' @details
#' The following tests are automatically done:
#' \describe{
#'  \item{`non_numeric`}{lat or lon are not numeric or cannot be converted to numeric}
#'  \item{`out_of_bounds`}{lat or lon are out of bounds (i.e., lat>90, lat<-90, lon>180, lon<-180)}
#'  \item{`null`}{lat or lon are null}
#' }
#'
#' The following tests can be selected:
#' \describe{
#'  \item{`zero`}{lat and lon are both zero}
#'  \item{`equal`}{lat and lon are equal}
#' }
#'
#' Countries are first parsed based on the country name in the record.
#' If no country name is given, it is guessed using the record's GPS coordinates.
#'
#' @return Input data.frame with the following output fields appended:
#'
#' \describe{
#' \item{`lat_parsed`}{Numeric. Parsed decimal latitude.}
#' \item{`lon_parsed`}{Numeric. Parsed decimal longitude.}
#' \item{`lat_res`}{Integer. Latitude resolution based on number of decimal places.}
#' \item{`lon_res`}{Integer. Longitude resolution based on number of decimal places.}
#' \item{`coordinate_error`}{Character. First error deteced when cleaning coordinates.}
#' \item{`country_parsed`}{Character. Parsed country. Synonyms are harmonized.}
#' \item{`sov_parsed`}{Character. Parsed sovereignty. Based on `country_parsed`.}
#' \item{`iso3_parsed`}{Character. Parsed iso-3 codes (ISO 3166-1 alpha-3 codes are three-letter country codes). Based on `country_parsed`.}
#' \item{`continent_parsed`}{Character. Parsed continent. Based on `country_parsed`.}
#' \item{`closest_country_distance`}{Numeric. Distance (in meters) between GPS coordinates and closest country. Only assigned if country cannot be parsed from supplied country name, and if record is not within the bounds of any country.}
#' }
#' @note Input coordinates are assumed to use WGS84 coordinate reference system.
#'
#' @export
#'
#' @examples
#' library(fungarium)
#' data(agaricales) #import sample data set
#' clean_geo <- clean_geography(as_dwca(agaricales)) #clean geo
#'

clean_geography <- function(data,
                      tests=c("zero", "equal")){

  # check args
  if (!inherits(data, "dwca")) {
    stop("'data' must be of class 'dwca'. Use `as_dwca()` first.")
  }
  checkmate::assertCharacter(tests)
  lapply(tests, checkmate::assert_choice, choices=c("zero", "equal"), .var.name='tests')

  # get attributes
  input_attributes <- attributes(data)

  # make output data frame
  data$lat_parsed <- as.numeric(NA)
  data$lon_parsed <- as.numeric(NA)
  data$lon_res <- as.integer(NA)
  data$lat_res <- as.integer(NA)
  data$coordinate_error <- as.character(NA)
  data$country_parsed <- as.character(NA)
  data$sov_parsed <- as.character(NA)
  data$iso3_parsed <- as.character(NA)
  data$continent_parsed <- as.character(NA)
  data$closest_country_distance <- as.numeric(NA)

  # make lat and lon numeric and detect resolution
  data$lon_parsed <- suppressWarnings(as.numeric(data$decimalLongitude))
  data$lat_parsed <- suppressWarnings(as.numeric(data$decimalLatitude))
  data$lon_res <- decimal_places(data$lon_parsed)
  data$lat_res <- decimal_places(data$lat_parsed)

  #perform null test
  null_bool <- is.na(data$decimalLongitude)|is.na(data$decimalLatitude)|data$decimalLongitude==""|data$decimalLatitude==""
  cat(sum(null_bool), " records found with null coordinates...\n")
  if (T %in% null_bool){
    data[null_bool,]$coordinate_error <- "null"
    data[null_bool,]$lat_parsed <- NA
    data[null_bool,]$lon_parsed <- NA
  }
  rm(null_bool)

  #perform non-numeric test
  non_numeric_bool <- is.na(data$lon_parsed)|is.na(data$lat_parsed) # TODO sub commas for periods before non-numeric test to allow for alt decimal convention
  non_numeric_bool <- non_numeric_bool&is.na(data$coordinate_error)
  cat(sum(non_numeric_bool), " records found with non-numeric coordinates...\n")
  if (T %in% non_numeric_bool){
    data[non_numeric_bool,]$coordinate_error <- "non_numeric"
    data[non_numeric_bool,]$lat_parsed <- NA
    data[non_numeric_bool,]$lon_parsed <- NA
  }
  rm(non_numeric_bool)

  #perform out_of_bounds coordinate test
  out_of_bounds_bool <- data$lat_parsed>90|data$lat_parsed<(-90)|data$lon_parsed>180|data$lon_parsed<(-180)
  out_of_bounds_bool <- out_of_bounds_bool&is.na(data$coordinate_error)
  cat(sum(out_of_bounds_bool), " records found with out-of-bounds coordinates...\n")
  if (T %in% out_of_bounds_bool){
    data[out_of_bounds_bool,]$coordinate_error <- "out_of_bounds"
    data[out_of_bounds_bool,]$lat_parsed <- NA
    data[out_of_bounds_bool,]$lon_parsed <- NA
  }
  rm(out_of_bounds_bool)

  #perform zero test
  if ("zero" %in% tests){
    zero_bool <- data$lat_parsed==0&data$lon_parsed==0
    zero_bool <- zero_bool&is.na(data$coordinate_error)
    cat(sum(zero_bool), " records found with zero coordinates...\n")
    if (T %in% zero_bool){
      data[zero_bool,]$coordinate_error <- "zero"
      data[zero_bool,]$lat_parsed <- NA
      data[zero_bool,]$lon_parsed <- NA
    }
    rm(zero_bool)
  }

  #perform equal test
  if ("equal" %in% tests){
    equal_bool <- data$lat_parsed==data$lon_parsed
    equal_bool <- equal_bool&is.na(data$coordinate_error)
    cat(sum(equal_bool), " records found with equal coordinates...\n")
    if (T %in% equal_bool){
      data[equal_bool,]$coordinate_error <- "equal"
      data[equal_bool,]$lat_parsed <- NA
      data[equal_bool,]$lon_parsed <- NA
    }
    rm(equal_bool)
  }

  # harmonize country names
  country_not_na_bool <- !is.na(data$country)
  cat(sum(country_not_na_bool), " records found with country name...\n")
  if (T%in%country_not_na_bool){
    cat("Parsing country names...\n")
    parsed_geo <- parse_geo_names(data[,c("country", "stateProvince")])
    data$country_parsed[country_not_na_bool] <- parsed_geo$country_parsed[country_not_na_bool]
    data$sov_parsed[country_not_na_bool] <- parsed_geo$sov_parsed[country_not_na_bool]
    data$iso3_parsed[country_not_na_bool] <- parsed_geo$iso3_parsed[country_not_na_bool]
    data$continent_parsed[country_not_na_bool] <- parsed_geo$continent_parsed[country_not_na_bool]
  }
  rm(country_not_na_bool)

  # predict blank countries based on on lat/lon
  country_na_bool <- !is.na(data$lat_parsed)&!is.na(data$lon_parsed)&is.na(data$country_parsed)
  cat(sum(country_na_bool), " records found with coordinates but no parsable country name...\n")
  if (T%in%country_na_bool){
    cat("Predicting country names based on coordinates...\n")
    parsed_geo <- parse_geo_names_from_coords(data[,c("lat_parsed", "lon_parsed", "country_parsed")])
    data$country_parsed[country_na_bool] <- parsed_geo$country_parsed[country_na_bool]
    data$sov_parsed[country_na_bool] <- parsed_geo$sov_parsed[country_na_bool]
    data$iso3_parsed[country_na_bool] <- parsed_geo$iso3_parsed[country_na_bool]
    data$continent_parsed[country_na_bool] <- parsed_geo$continent_parsed[country_na_bool]
    data$closest_country_distance[country_na_bool] <- parsed_geo$closest_country_distance[country_na_bool]
  }
  rm(country_na_bool)

  # add cleaning attributes
  attributes_to_copy <- input_attributes[!names(input_attributes) %in% c("names", "row.names")]
  attributes(data) <- c(attributes(data)[names(attributes(data)) %in% c("names", "row.names")], attributes_to_copy)
  attr(data, "clean_geography") <- TRUE

  # return output
  return(data)
}

##################################
# Helpers
##################################

# get the number of decimal places from coordinates
decimal_places <- function(x) {
  x_str <- gsub("0+$", "", gsub("^.*\\.", "", x))
  nchar(x_str)
}

# parse geographic location names
parse_geo_names <- function(data){
  # check args
  checkmate::assert_data_frame(data)
  checkmate::assert_true("country"%in%colnames(data))
  checkmate::assert_true("stateProvince"%in%colnames(data))

  # clean up countries reference data
  countries_ref <- sf::st_drop_geometry(rnaturalearth::ne_countries(scale="large", type = "countries"))
  # countries_ref <- dplyr::distinct(rbind(countries_ref, sf::st_drop_geometry(rnaturalearth::ne_countries(scale="small", type = "countries"))))
  continents <- countries_ref$continent # this preserves the continents list for parsing continent later (not the smoothest approach here)
  col_bool <- !(sapply(countries_ref, is.numeric))
  col_bool <- col_bool&!(colnames(countries_ref)%in%c("sovereignt", "type", "economy", "income_grp", "woe_note", "continent", "region_un", "subregion", "region_wb"))
  countries_ref <- countries_ref[,col_bool]
  countries_ref_clean <- as.data.frame(lapply(countries_ref, str_clean, periods = ""), stringsAsFactors = FALSE)

  # make unique list of countries from input
  country_u <- unique(data$country)
  country_u_df <- data.frame(country_u = country_u,  country_u_clean=str_clean(country_u, periods = ""))

  # create new output cols
  country_u_df$country_parsed <- rep(NA, nrow(country_u_df))
  country_u_df$iso3_parsed <- rep(NA, nrow(country_u_df))
  country_u_df$sov_parsed <- rep(NA, nrow(country_u_df))
  country_u_df$continent_parsed <- rep(NA, nrow(country_u_df))

  # iterate through each country and assign standardized name from ref data
  for (i in seq_len(nrow(country_u_df))){
    if (!is.na(country_u_df$country_u_clean[i])){
      for (j in seq_len(ncol(countries_ref))){ # iterate through each column in rnaturalearth data frame; start with admin col (col#10)
        match_bool <- country_u_df$country_u_clean[i] == countries_ref_clean[,j]
        match_bool[is.na(match_bool)] <- F
        if (T%in%match_bool){
          country_parsed <- unique(countries_ref$admin[match_bool])
          country_u_df$country_parsed[i] <- ifelse(length(country_parsed)>1, NA, country_parsed)
          sov_parsed <- unique(countries_ref$sov_a3[match_bool])
          country_u_df$sov_parsed[i] <- ifelse(length(sov_parsed)>1, NA, sov_parsed)
          iso3_parsed <- unique(countries_ref$adm0_a3[match_bool])
          country_u_df$iso3_parsed[i] <- ifelse(length(iso3_parsed)>1, NA, iso3_parsed)
          continent_parsed <- unique(continents[match_bool])
          country_u_df$continent_parsed[i] <- ifelse(length(continent_parsed)>1, NA, continent_parsed)
          break
        }
      }
    }
  }

  # map back to original rows using match (fast and avoids joins that copy entire data frames)
  idx_map <- match(data$country, country_u_df$country_u)
  out <- data.frame(
    country_parsed = country_u_df$country_parsed[idx_map],
    sov_parsed = country_u_df$sov_parsed[idx_map],
    iso3_parsed = country_u_df$iso3_parsed[idx_map],
    continent_parsed = country_u_df$continent_parsed[idx_map]
  )

  return(out)
}


parse_geo_names_from_coords <- function(data){
  # check args
  checkmate::assert_data_frame(data)
  checkmate::assert_true("lat_parsed"%in%colnames(data))
  checkmate::assert_true("lon_parsed"%in%colnames(data))
  checkmate::assert_true("country_parsed"%in%colnames(data))

  #-----------------------------------------------------------------------------
  # countries
  #-----------------------------------------------------------------------------
  # get records with coords but no country
  country_na_bool <- !is.na(data$lat_parsed)&!is.na(data$lon_parsed)&is.na(data$country_parsed)

  if (T%in%country_na_bool){
    # get country geometry data
    countries_shp <- rnaturalearth::ne_countries(scale="large", type = "countries")
    countries_shp <- sf::st_make_valid(countries_shp) # Fix invalid geometries
    keep_cols <- c("admin", "adm0_a3", "sov_a3", "continent")
    countries_shp <- countries_shp[,keep_cols]

    # prep unique set of coords to test
    country_na <- dplyr::distinct(data[country_na_bool, c("lat_parsed", "lon_parsed")])
    country_na <- sf::st_as_sf(country_na, coords = c("lon_parsed", "lat_parsed"), crs = sf::st_crs(countries_shp), remove=F)

    # join countries
    within_country <- as.integer(sf::st_intersects(country_na, countries_shp, prepared=T)) #TODO use dist arg (st_is_within_dist)
    country_na$country_parsed <- countries_shp$admin[within_country]
    country_na$iso3_parsed <- countries_shp$adm0_a3[within_country]
    country_na$sov_parsed <- countries_shp$sov_a3[within_country]
    country_na$continent_parsed <- countries_shp$continent[within_country]

    # create output
    match_idx <- match(paste0(data$lat_parsed, "_", data$lon_parsed), paste0(country_na$lat_parsed, "_", country_na$lon_parsed))
    data$country_parsed[country_na_bool] <- country_na$country_parsed[match_idx[country_na_bool]]
    data$iso3_parsed[country_na_bool] <- country_na$iso3_parsed[match_idx[country_na_bool]]
    data$sov_parsed[country_na_bool] <- country_na$sov_parsed[match_idx[country_na_bool]]
    data$continent_parsed[country_na_bool] <- country_na$continent_parsed[match_idx[country_na_bool]]
  }

  # get closest country for points not 'within' country geometry
  country_na_bool <- !is.na(data$lat_parsed)&!is.na(data$lon_parsed)&is.na(data$country_parsed)
  cat(sum(country_na_bool), " records found with coordinates and no parsable country name and are not within any country boundaries...\n")

  if (T%in%country_na_bool){
    # prep unique set of coords to test
    country_na <- dplyr::distinct(data[country_na_bool, c("lat_parsed", "lon_parsed")])
    country_na <- sf::st_as_sf(country_na, coords = c("lon_parsed", "lat_parsed"), crs = sf::st_crs(countries_shp), remove=F)

    # Disable s2 for faster planar ops
    sf::sf_use_s2(FALSE)

    # Reproject to a global equal-area CRS (meters, planar)
    countries_shp <- sf::st_transform(countries_shp, 8857)
    country_na <- sf::st_transform(country_na, 8857)

    # Simplify geometry to reduce vertices (keep X% of detail)
    countries_shp <- rmapshaper::ms_simplify(countries_shp, keep = 0.50, keep_shapes = TRUE)


    # get closest feature
    cat("Finding countries closest to coordinates...\n")
    closest_country_index <- sf::st_nearest_feature(sf::st_geometry(country_na), sf::st_geometry(countries_shp))
    country_na$country_parsed <- countries_shp$admin[closest_country_index]
    country_na$iso3_parsed <- countries_shp$adm0_a3[closest_country_index]
    country_na$sov_parsed <- countries_shp$sov_a3[closest_country_index]
    country_na$continent_parsed <- countries_shp$continent[closest_country_index]
    country_na$closest_country_distance <- as.numeric(NA)

    # get distance to closest feature
    cat("Calculating distance to closest countries...\n")
    for (i in seq(1, nrow(country_na), by=1000)){# split up data for distance calcs to reduce memory consumption
      idx_seq <- i:min(i+999, nrow(country_na))
      country_na$closest_country_distance[idx_seq] <- sf::st_distance(sf::st_geometry(country_na)[idx_seq], sf::st_geometry(countries_shp)[closest_country_index[idx_seq]], by_element = TRUE)
      # progress bar
      cat("[", paste0(rep("=", floor((max(idx_seq)/nrow(country_na))*20)), collapse=""), paste0(rep(" ", 20 - floor((max(idx_seq)/nrow(country_na))*20)), collapse=""), "] ", ceiling(max(idx_seq)/nrow(country_na)*100), "%\r", sep = "")
    }
    cat("\n")
    # country_na$closest_country_distance <- sf::st_distance(sf::st_geometry(country_na), sf::st_geometry(countries_shp)[closest_country_index], by_element = TRUE)
    country_na <- sf::st_drop_geometry(country_na)
    rm(countries_shp)

    # Re-enable s2
    sf::sf_use_s2(TRUE)

    # create output
    match_idx <- match(paste0(data$lat_parsed, "_", data$lon_parsed), paste0(country_na$lat_parsed, "_", country_na$lon_parsed))
    data$country_parsed[country_na_bool] <- country_na$country_parsed[match_idx[country_na_bool]]
    data$iso3_parsed[country_na_bool] <- country_na$iso3_parsed[match_idx[country_na_bool]]
    data$sov_parsed[country_na_bool] <- country_na$sov_parsed[match_idx[country_na_bool]]
    data$continent_parsed[country_na_bool] <- country_na$continent_parsed[match_idx[country_na_bool]]
    data$closest_country_distance <- rep(NA, nrow(data))
    data$closest_country_distance[country_na_bool] <- country_na$closest_country_distance[match_idx[country_na_bool]]
  }



  # return output
  return(data[,c("country_parsed", "sov_parsed", "iso3_parsed", "continent_parsed", "closest_country_distance")])
}


# find_closest_country <- function(data, countries_shp){
#   # check args
#   checkmate::assert_data_frame(data)
#   checkmate::assert_true("lat_parsed"%in%colnames(data))
#   checkmate::assert_true("lon_parsed"%in%colnames(data))
#   checkmate::assert_true("country_parsed"%in%colnames(data))
#
#   # get records with coords but no country
#   country_na_bool <- !is.na(data$lat_parsed)&!is.na(data$lon_parsed)&is.na(data$country_parsed)
#
#   if (T%in%country_na_bool){
#     # prep unique set of coords to test
#     country_na <- dplyr::distinct(data[country_na_bool, c("lat_parsed", "lon_parsed")])
#     country_na <- sf::st_as_sf(country_na, coords = c("lon_parsed", "lat_parsed"), crs = sf::st_crs(countries_shp), remove=F)
#
#     # get closest feature
#     closest_country_index <- sf::st_nearest_feature(country_na, countries_shp)
#     country_na$country_parsed <- countries_shp$admin[closest_country_index]
#     country_na$iso3_parsed <- countries_shp$adm0_a3[closest_country_index]
#     country_na$sov_parsed <- countries_shp$sov_a3[closest_country_index]
#     country_na$continent_parsed <- countries_shp$continent[closest_country_index]
#     country_na$closest_country_distance <- sf::st_distance(country_na, countries_shp[closest_country_index,], by_element = TRUE)
#     country_na <- sf::st_drop_geometry(country_na)
#
#     # create output
#     match_idx <- match(paste0(data$lat_parsed, "_", data$lon_parsed), paste0(country_na$lat_parsed, "_", country_na$lon_parsed))
#     data$country_parsed[country_na_bool] <- country_na$country_parsed[match_idx[country_na_bool]]
#     data$iso3_parsed[country_na_bool] <- country_na$iso3_parsed[match_idx[country_na_bool]]
#     data$sov_parsed[country_na_bool] <- country_na$sov_parsed[match_idx[country_na_bool]]
#     data$continent_parsed[country_na_bool] <- country_na$continent_parsed[match_idx[country_na_bool]]
#     data$closest_country_distance <- rep(NA, nrow(data))
#     data$closest_country_distance[country_na_bool] <- country_na$closest_country_distance[match_idx[country_na_bool]]
#   }
#
#   # return output
#   return(data[,c("country_parsed", "sov_parsed", "iso3_parsed", "continent_parsed", "closest_country_distance")])
#
# }
