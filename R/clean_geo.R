#' @title Clean and harmonize heterogeneous geographic data (lat/lon/country/state/province)
#'
#' @description
#' Parse varying formats/spellings for geographic data into a consistent formats
#' and check the validity of lat/lon coordinates using various tests
#'
#'
#' @param lat Character. Latitude.
#' @param lon Character. Longitude.
#' @param country Character. Name of the country.
#' @param state_province Character. Name of the state/province.
#' @param tests Character. Coordinate cleaning tests to perform. Options include: "zero", "equal", "countries", "centroids", "all". Default is "all".
#' @param centroid_dis Integer. Distance threshold (in meters) to use for the centroid test. Default is 100.
#' @param round_digits Integer. Number of decimal places to use for rounding coordinates. Default is 4. If NULL, no rounding is performed.
#'
#' @details The following tests are automatically done:
#' \describe{
#'  \item{\code{non_numeric}}{lat or lon are not numeric or cannot be converted to numeric}
#'  \item{\code{out_of_bounds}}{lat or lon are out of bounds (i.e., lat>90, lat<-90, lon>180, lon<-180)}
#'  \item{\code{null}}{lat or lon are null}
#' }
#' The following tests can be selected:
#' \describe{
#'  \item{\code{zero}}{lat and lon are both zero}
#'  \item{\code{equal}}{lat and lon are equal}
#'  \item{\code{countries}}{point is outside the bounds of the country listed}
#'  \item{\code{centroid}}{distance between point and country centroid is less than or equal to the centroid_dis specified}
#' }
#' @return Data.frame with the following output fields.
#'
#' \describe{
#' \item{\code{lat_raw}}{Character. Input latitude.}
#' \item{\code{lon_raw}}{Character. Input longitude.}
#' \item{\code{country_raw}}{Character. Input country.}
#' \item{\code{state_province_raw}}{Character. Input state/province.}
#' \item{\code{lat_parsed}}{Numeric. Parsed decimal latitude.}
#' \item{\code{lon_parsed}}{Numeric. Parsed decimal longitude.}
#' \item{\code{country_parsed}}{Character. Parsed country. Synonyms are harmonized.}
#' \item{\code{state_province_parsed}}{Character. Parsed state/province. Synonyms are harmonized.}
#' \item{\code{lat_res}}{Integer. Latitude resolution based on number of decimal places.}
#' \item{\code{lon_res}}{Integer. Longitude resolution based on number of decimal places.}
#' \item{\code{coordinate_error}}{Character. First error deteced when cleaning coordinates.}
#'
#' @note Input coordinates are assumed to use WGS84 coordinate reference system.
#'
#' @export
#'
#' @examples
#' library(fungarium)
#' data(agaricales_updated) #import sample data set
#' clean_coordinates <- clean_coordinates(agaricales_updated) #clean coordinates
#'

clean_geo <- function(lat,
                      lon,
                      country,
                      state_province,
                      tests="all",
                      centroid_dis=100L){

  # check args
  checkmate::assert_character(lat)
  checkmate::assert_character(lon, len = length(lat))
  checkmate::assert_character(country, len = length(lat))
  checkmate::assert_character(state_province, len = length(lat))
  checkmate::assertCharacter(tests)
  lapply(tests, checkmate::assert_choice, choices=c("all", "zero", "equal", "countries", "centroids"), .var.name='tests')
  checkmate::assert_choice(tests, c("all", "zero", "equal", "countries", "centroids"))
  checkmate::assert_integer(centroid_dis, len=1)

  if(tests=="all"){
    tests <- c("zero", "equal", "countries", "centroids")
  }
  if(!is.null(centroid_dis)){
    centroid_dis <- units::as_units(centroid_dis, "m")
  }

  # reference data
  # countries_shp <- rnaturalearth::ne_countries('large', returnclass = "sf") # import world countries_shp file
  countries_shp <- rnaturalearthhires::countries10
  countries_shp <- sf::st_make_valid(countries_shp) # Fix invalid geometries

  # make ouput data frame
  out <- data.frame(lat_raw = lat, lon_raw = lon, country_raw = country, state_province_raw = state_province)
  out$lat_parsed <- as.numeric(NA)
  out$lon_parsed <- as.numeric(NA)
  # out$country_parsed <- as.character(NA)
  # out$state_province_parsed <- as.character(NA)
  # out$sov_parsed <- as.character(NA)
  out$lon_res <- as.integer(NA)
  out$lat_res <- as.integer(NA)
  out$coordinate_error <- as.character(NA)
  # out$parsing_error <- as.character(NA)


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

  # harmonize country names and state/province names
  out <- cbind(out, parse_country(out))

  # predict blank countries and state province based on on lat/lon
  out2 <- parse_geo_names_from_coords(out[1:100,])

  # #convert lat long points to sf geometry in cea coordinate space
  if (T%in%(c("countries","centroids" ) %in% tests)){
    out_sf <- dplyr::distinct(out[,c("lat_parsed", "lon_parsed", "country_parsed")])
    out_sf <- out_sf[!is.na(out_sf$lat_parsed)&!is.na(out_sf$lon_parsed)&!is.na(out_sf$country_parsed),]
    out_sf <- sf::st_as_sf(out_sf, coords = c("lon_parsed", "lat_parsed"), crs = sf::st_crs(countries_shp)) # convert lat/lon to sf points
  }

  #perform countries test
  if ("countries" %in% tests){
    message("Running 'countries' test...")
    within_country <- sf::st_intersects(out_sf, countries_shp, prepared=T)
    within_country <- as.integer(within_country)
    country_match <- countries_shp$admin[within_country]==out_sf$country_parsed
    out_sf$coordinate_error <- ifelse(country_match, NA, "out_of_country")
    within<- within-1 #shapefile row labels start at 0
    data$row_numb <- within
    rm(within)
    countries_shp2 <- as.data.frame(countries_shp)
    countries_shp2 <- countries_shp2[,!colnames(countries_shp2)%in%"geometry"]
    countries_shp2$row_numb <- 0:(nrow(countries_shp2)-1) #shapefile row labels start at 0
    data2 <- as.data.frame(data)
    data2 <- data2[,c("row_numb",country)]
    data2 <- dplyr::left_join(data2, countries_shp2, by="row_numb")
    check <- lapply(as.list(as.data.frame(t(data2))),
                    country_check)

    rm(countries_shp2,data2)
    check <- as.logical(check)
    data <- data[check,]
    message(paste0("'countries' test: ", (row1-nrow(data)), " records removed."))
  }
  #
  # #perform centroids test
  # if ("centroids" %in% tests){
  #   message("Running 'centroids' test...")
  #   row1 <- nrow(data)
  #   centroids <- sf::st_centroid(countries_shp$geometry)
  #   centroids <- centroids[data$row_numb+1]#shapefile row labels start at 0
  #   points <- data$geometry
  #   check <- sf::st_distance(points,centroids, by_element = T)>centroid_dis
  #   data <- data[check,]
  #   message(paste0("'centroids' test: ", (row1-nrow(data)), " records removed."))
  # }
  # message(paste("Total records removed:", row0-nrow(data)))
  #
  # # Extract coordinates from geometry
  # coords <- sf::st_coordinates(data)
  #
  # # Add coordinates as separate lat/long columns
  # data <- cbind(data, coords)
  #
  # # Drop geometry column:
  # data <- sf::st_drop_geometry(data)
  #
  # # rename the coordinate columns
  # colnames(data)[(ncol(data)-1):ncol(data)] <- c("longitude_fixed", "latitude_fixed")
  #
  # #return cleaned output
  # return(data[,!colnames(data)=="row_numb"])


  return(out)
}


#countries test helper function
country_check <- function(x){
  x <- str_clean(x)
  out <- x[2]%in%x[3:length(x)]
  return(out)
}

decimal_places <- function(x) {
  x_str <- gsub("0+$", "", gsub("^.*\\.", "", x))
  nchar(x_str)
}




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

  # clean up state/province reference data
  states_ref <- sf::st_drop_geometry(rnaturalearth::ne_states())
  col_bool <- !(sapply(states_ref, is.numeric))
  states_ref <- states_ref[,col_bool]
  states_ref_clean <- as.data.frame(sapply(states_ref, str_clean))

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


  # iterate through countries that couldn't be parsed to check for state_province
  non_matched_bool <- is.na(country_u_df$country_parsed)&!is.na(country_u_df$country_u_clean)
  non_match_countries <- country_u_df[non_matched_bool,]

  for (i in seq_len(nrow(non_match_countries))){
    for (j in seq_len(ncol(states_ref))){ # iterate through each column in rnaturalearth data frame; start with admin col (col#10)
      match_bool <- non_match_countries$country_u_clean[i] == states_ref_clean[,j]
      match_bool[is.na(match_bool)] <- F
      if (T%in%match_bool){
        # print(paste0(non_match_countries$country_u_clean[i], "...", states_ref$admin[match_bool]))
        country_parsed <- unique(states_ref$admin[match_bool])
        non_match_countries$country_parsed[i] <- ifelse(length(country_parsed)>1, NA, country_parsed)
        sov_parsed <- unique(states_ref$sov_a3[match_bool])
        non_match_countries$sov_parsed[i] <- ifelse(length(sov_parsed)>1, NA, sov_parsed)
        iso3_parsed <- unique(states_ref$adm0_a3[match_bool])
        non_match_countries$iso3_parsed[i] <- ifelse(length(iso3_parsed)>1, NA, iso3_parsed)
        break
      }
    }
  }

  country_u_df[non_matched_bool,]$country_parsed <- non_match_countries$country_parsed
  country_u_df[non_matched_bool,]$sov_parsed <- non_match_countries$sov_parsed
  country_u_df[non_matched_bool,]$iso3_parsed <- non_match_countries$iso3_parsed

  # create output df
  out <- dplyr::left_join(data[,c("country_raw", "state_province_raw")], country_u_df, by = dplyr::join_by(country_raw == country_u))

  # parse state/province names
  out <- parse_state_province(out)

  # return output
  return(out)
}

parse_state_province <- function(data, predict_empty_countries = T){
    # check args
    checkmate::assert_data_frame(data)
    checkmate::assert_true("country_raw"%in%colnames(data))
    checkmate::assert_true("state_province_raw"%in%colnames(data))
    checkmate::assert_true("country_parsed"%in%colnames(data))
    checkmate::assert_true("sov_parsed"%in%colnames(data))
    checkmate::assert_true("iso3_parsed"%in%colnames(data))

    # clean up state/province reference data
    states_ref <- sf::st_drop_geometry(rnaturalearth::ne_states())
    col_bool <- !(sapply(states_ref, is.numeric))
    states_ref <- states_ref[,col_bool]
    states_ref_clean <- as.data.frame(sapply(states_ref, str_clean))

    # make unique list of states from input
    state_u <- unique(data$state_province_raw)
    state_u_df <- data.frame(state_u = state_u,  state_u_clean=str_clean(state_u, periods = ""))
    state_u_df$state_province_parsed <- rep(NA, nrow(state_u_df))
    state_u_df$iso_3166_2_parsed <- rep(NA, nrow(state_u_df))
    state_u_df$country_parsed <- rep(NA, nrow(state_u_df))
    state_u_df$iso3_parsed<- rep(NA, nrow(state_u_df))
    state_u_df$sov_parsed <- rep(NA, nrow(state_u_df))

    # iterate through states/provinces
    for (i in seq_len(nrow(state_u_df))){
      for (j in seq_len(ncol(states_ref))){ # iterate through each column in rnaturalearth data frame; start with admin col (col#10)
        match_bool <- state_u_df$state_u_clean[i] == states_ref_clean[,j]
        match_bool[is.na(match_bool)] <- F
        if (T%in%match_bool){
          # print(paste0(state_u_df$state_u_clean[i], "...", states_ref$name[match_bool]))
          state_province_parsed <- unique(states_ref$name[match_bool])
          state_u_df$state_province_parsed[i] <- ifelse(length(state_province_parsed)>1, NA, state_province_parsed)
          iso_3166_2_parsed <- unique(states_ref$iso_3166_2[match_bool])
          state_u_df$iso_3166_2_parsed[i] <- ifelse(length(iso_3166_2_parsed)>1, NA, iso_3166_2_parsed)
          if (predict_empty_countries){
            country_parsed <- unique(states_ref$admin[match_bool])
            state_u_df$country_parsed[i] <- ifelse(length(country_parsed)>1, NA, country_parsed)
            iso3_parsed <- unique(states_ref$adm0_a3[match_bool])
            state_u_df$iso3_parsed[i] <- ifelse(length(iso3_parsed)>1, NA, iso3_parsed)
            sov_parsed <- unique(states_ref$sov_a3[match_bool])
            state_u_df$sov_parsed[i] <- ifelse(length(sov_parsed)>1, NA, sov_parsed)
          }
          break
        }
      }
    }

    # create output df
    out <- dplyr::left_join(data[,c("country_raw", "state_province_raw")], state_u_df, by = dplyr::join_by(state_province_raw==state_u))
    out[!is.na(data$country_parsed),]$country_parsed <- data$country_parsed[!is.na(data$country_parsed)] # add back original country data where it wasn't NA
    out[!is.na(data$iso3_parsed),]$iso3_parsed <- data$iso3_parsed[!is.na(data$iso3_parsed)] # add back original country data where it wasn't NA
    out[!is.na(data$sov_parsed),]$sov_parsed <- data$sov_parsed[!is.na(data$sov_parsed)] # add back original country data where it wasn't NA

    # return parsed output
    return(out[,grepl("parsed", colnames(out))])
}

parse_geo_names_from_coords <- function(data){
  # check args
  checkmate::assert_data_frame(data)
  checkmate::assert_true("country_raw"%in%colnames(data))
  checkmate::assert_true("state_province_raw"%in%colnames(data))
  checkmate::assert_true("country_parsed"%in%colnames(data))
  checkmate::assert_true("sov_parsed"%in%colnames(data))
  checkmate::assert_true("iso3_parsed"%in%colnames(data))
  checkmate::assert_true("state_province_parsed"%in%colnames(data))
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
  }

  #-----------------------------------------------------------------------------
  # states
  #-----------------------------------------------------------------------------
  # get records with coords but no state/province
  state_na_bool <- !is.na(data$lat_parsed)&!is.na(data$lon_parsed)&is.na(data$state_province_parsed)

  if (T%in%state_na_bool){
    # get state/province geometry data
    states_shp <- rnaturalearthhires::states10
    states_shp <- sf::st_make_valid(states_shp) # Fix invalid geometries

    # prep unique set of coords to test
    state_na <- dplyr::distinct(data[state_na_bool, c("lat_parsed", "lon_parsed")])
    state_na <- sf::st_as_sf(state_na, coords = c("lon_parsed", "lat_parsed"), crs = sf::st_crs(countries_shp), remove=F)

    # join states
    within_state <- as.integer(sf::st_intersects(state_na, states_shp, prepared=T))
    print(within_state)
    state_na$state_province_parsed <- states_shp$name[within_state]
    state_na$iso_3166_2_parsed <- states_shp$iso_3166_2[within_state]

    # create output df
    out2 <- dplyr::left_join(data[state_na_bool,c("lat_parsed", "lon_parsed")], state_na, by = c("lat_parsed", "lon_parsed"))
    data[state_na_bool,]$state_province_parsed <- out2[state_na_bool,]$state_province_parsed
    data[state_na_bool,]$iso_3166_2_parsed <- out2[state_na_bool,]$iso_3166_2_parsed
  }

  # return output
  return(data)
}


# TODO before using coords to guess countries/states, uses countries/states to guess if coords are flipped on accident
# ex: 39.648
# -77.467
# NA
# Maryland
# 39.648000
# -77.46700
# 3
# 3
# NA
# Maryland
# NA
# Mexico
# MEX
# MEX


