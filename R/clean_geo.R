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
#' The following tests can be selected:
#' \describe{
#'  \item{`zero`}{lat and lon are both zero}
#'  \item{`equal`}{lat and lon are equal}
#' }
#' @return Data.frame with the following output fields:
#'
#' \describe{
#' \item{`lat_raw`}{Character. Input latitude.}
#' \item{`lon_raw`}{Character. Input longitude.}
#' \item{`country_raw`}{Character. Input country.}
#' \item{`lat_parsed`}{Numeric. Parsed decimal latitude.}
#' \item{`lon_parsed`}{Numeric. Parsed decimal longitude.}
#' \item{`lat_res`}{Integer. Latitude resolution based on number of decimal places.}
#' \item{`lon_res`}{Integer. Longitude resolution based on number of decimal places.}
#' \item{`coordinate_error`}{Character. First error deteced when cleaning coordinates.}
#' \item{`country_parsed`}{Character. Parsed country. Synonyms are harmonized.}
#' \item{`continent_parsed`}{Character. Parsed continent. Based on `country_parsed`.}
#'
#' @note Input coordinates are assumed to use WGS84 coordinate reference system.
#'
#' @export
#'
#' @examples
#' library(fungarium)
#' data(agaricales_updated) #import sample data set
#' agaricales_updated <- as_dwca(agaricales_updated) # convert to dwca object
#' agaricales_geo_clean <- clean_geo(agaricales_updated) #clean coordinates
#'

clean_geo <- function(data,
                      tests=c("zero", "equal")){

  # check args
  if (!inherits(data, "dwca")) {
    stop("'data' must be of class 'dwca'. Use `as_dwca()` first.")
  }
  checkmate::assertCharacter(tests)
  lapply(tests, checkmate::assert_choice, choices=c("zero", "equal"), .var.name='tests')

  # make output data frame
  out <- data.frame(lat_raw = data$decimalLatitude, lon_raw = data$decimalLongitude, country_raw = data$country, state_province_raw = data$stateProvince)
  out$lat_parsed <- as.numeric(NA)
  out$lon_parsed <- as.numeric(NA)
  out$lon_res <- as.integer(NA)
  out$lat_res <- as.integer(NA)
  out$coordinate_error <- as.character(NA)

  # make lat and lon numeric and detect resolution
  out$lon_parsed <- as.numeric(out$lon_raw)
  out$lat_parsed <- as.numeric(out$lat_raw)
  out$lon_res <- decimal_places(out$lon_parsed)
  out$lat_res <- decimal_places(out$lat_parsed)

  #perform null test
  null_bool <- is.na(out$lon_raw)|is.na(out$lat_raw)|out$lon_raw==""|out$lat_raw==""
  if (T %in% null_bool){
    out[null_bool,]$coordinate_error <- "null"
  }

  #perform non-numeric test
  non_numeric_bool <- is.na(out$lon_parsed)|is.na(out$lat_parsed)
  non_numeric_bool <- non_numeric_bool&is.na(out$coordinate_error)
  if (T %in% non_numeric_bool){
    out[non_numeric_bool,]$coordinate_error <- "non_numeric"
  }

  #perform out_of_bounds coordinate teest
  out_of_bounds_bool <- out$lat_parsed>90|out$lat_parsed<(-90)|out$lon_parsed>180|out$lon_parsed<(-180)
  out_of_bounds_bool <- out_of_bounds_bool&is.na(out$coordinate_error)
  if (T %in% out_of_bounds_bool){
    out[out_of_bounds_bool,]$coordinate_error <- "out_of_bounds"
  }

  #perform zero test
  if ("zero" %in% tests){
    zero_bool <- out$lat_parsed==0&out$lon_parsed==0
    zero_bool <- zero_bool&is.na(out$coordinate_error)
    if (T %in% zero_bool){
      out[zero_bool,]$coordinate_error <- "zero"
    }
  }

  #perform equal test
  if ("equal" %in% tests){
    equal_bool <- out$lat_parsed==out$lon_parsed
    equal_bool <- equal_bool&is.na(out$coordinate_error)
    if (T %in% zero_bool){
      out[equal_bool,]$coordinate_error <- "equal"
    }
  }

  # harmonize country names
  out <- cbind(out, parse_geo_names(out))

  # predict blank countries and state province based on on lat/lon
  out <- parse_geo_names_from_coords(out)


  return(out)
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
  checkmate::assert_true("country_raw"%in%colnames(data))
  checkmate::assert_true("state_province_raw"%in%colnames(data))

  # clean up countries reference data
  countries_ref <- sf::st_drop_geometry(rnaturalearth::ne_countries())
  col_bool <- !(sapply(countries_ref, is.numeric))
  col_bool <- col_bool&!(colnames(countries_ref)%in%c("sovereignt", "type", "economy", "income_grp", "woe_note", "continent", "region_un", "subregion", "region_wb"))
  countries_ref <- countries_ref[,col_bool]
  countries_ref_clean <- as.data.frame(sapply(countries_ref, str_clean))

  # make unique list of countries from input
  country_u <- unique(data$country_raw)
  country_u_df <- data.frame(country_u = country_u,  country_u_clean=str_clean(country_u, periods = ""))

  # create new output cols
  country_u_df$country_parsed <- rep(NA, nrow(country_u_df))
  country_u_df$iso3_parsed <- rep(NA, nrow(country_u_df))
  country_u_df$sov_parsed <- rep(NA, nrow(country_u_df))

  # iterate through each country and assign standardized name from ref data
  for (i in seq_len(nrow(country_u_df))){
    for (j in seq_len(ncol(countries_ref))){ # iterate through each column in rnaturalearth data frame; start with admin col (col#10)
      match_bool <- country_u_df$country_u_clean[i] == countries_ref_clean[,j]
      match_bool[is.na(match_bool)] <- F
      if (T%in%match_bool){
        # print(paste0(country_u_df$country_u_clean[i], "...", countries_ref$admin[match_bool]))
        country_parsed <- unique(countries_ref$admin[match_bool])
        country_u_df$country_parsed[i] <- ifelse(length(country_parsed)>1, NA, country_parsed)
        sov_parsed <- unique(countries_ref$sov_a3[match_bool])
        country_u_df$sov_parsed[i] <- ifelse(length(sov_parsed)>1, NA, sov_parsed)
        iso3_parsed <- unique(countries_ref$adm0_a3[match_bool])
        country_u_df$iso3_parsed[i] <- ifelse(length(iso3_parsed)>1, NA, iso3_parsed)
        break
      }
    }
  }

  # create output df
  out <- dplyr::left_join(data[,c("country_raw", "state_province_raw")], country_u_df, by = dplyr::join_by(country_raw == country_u))

  # return output
  return(cbind(data,out))
}


parse_geo_names_from_coords <- function(data){
  # check args
  checkmate::assert_data_frame(data)
  checkmate::assert_true("country_raw"%in%colnames(data))
  checkmate::assert_true("country_parsed"%in%colnames(data))
  checkmate::assert_true("sov_parsed"%in%colnames(data))
  checkmate::assert_true("iso3_parsed"%in%colnames(data))
  checkmate::assert_true("iso_3166_2_parsed"%in%colnames(data))

  #-----------------------------------------------------------------------------
  # countries
  #-----------------------------------------------------------------------------
  # get records with coords but no country
  country_na_bool <- !is.na(data$lat_parsed)&!is.na(data$lon_parsed)&is.na(data$country_parsed)

  if (T%in%country_na_bool){
    # get country geometry data
    countries_shp <- rnaturalearthhires::countries10
    countries_shp <- sf::st_make_valid(countries_shp) # Fix invalid geometries

    # prep unique set of coords to test
    country_na <- dplyr::distinct(data[country_na_bool, c("lat_parsed", "lon_parsed")])
    country_na <- sf::st_as_sf(country_na, coords = c("lon_parsed", "lat_parsed"), crs = sf::st_crs(countries_shp), remove=F)

    # join countries
    within_country <- as.integer(sf::st_intersects(country_na, countries_shp, prepared=T))
    print(within_country)
    print(length(within_country))
    print(nrow(country_na))
    country_na$country_parsed <- countries_shp$ADMIN[within_country]
    country_na$iso3_parsed <- countries_shp$ADM0_A3[within_country]
    country_na$sov_parsed <- countries_shp$SOV_A3[within_country]

    # create output
    out1 <- dplyr::left_join(data[country_na_bool,c("lat_parsed", "lon_parsed")], country_na, by = c("lat_parsed", "lon_parsed"))
    data[country_na_bool,]$country_parsed <- out1[country_na_bool,]$country_parsed
    data[country_na_bool,]$iso3_parsed <- out1[country_na_bool,]$iso3_parsed
    data[country_na_bool,]$sov_parsed <- out1[country_na_bool,]$sov_parsed
    data[country_na_bool,]$continent_parsed <- out1[country_na_bool,]$continent_parsed

  }

  # return output
  return(data)
}
