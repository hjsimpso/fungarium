#' Clean lat/long coordinates in occurrence data sets
#'
#' Removes records that do not pass various coordinate validity tests.
#'
#'
#' @param data Data.frame of occurrence data (e.g. MyCoPortal, GBIF) that includes decimal latitude and longitude
#' @param lat Character string specifying the decimal latitude column. Default is "decimalLatitude" (i.e., the Darwin Core standard).
#' @param lon Character string specifying the decimal longitude column. Default is "decimalLongitude" (i.e., the Darwin Core standard).
#' @param tests Character vector specifying the coordinate cleaning tests to perform. Options include: "zero", "equal", "countries", "centroids". Default is c("zero", "equal", "countries", "centroids").
#' @param country Character string specifying the name of the country column. Default is "country" (i.e., the Darwin Core standard).
#' @param centroid_dis Numeric specifying the distance threshold (in meters) to use for the centroid test. Default is 100.
#' @param round_digits Integer specifying the number of decimal places to use for rounding coordinates. Default is 4. If NULL, no rounding is performed.
#'
#' @details The following tests are automatically done:
#' \describe{
#'  \item{\code{non-numeric}}{lat or lon are not numeric or cannot be converted to numeric}
#'  \item{\code{non-valid}}{lat or lon are invalid numbers (i.e., lat>90, lat<-90, lon>180, lon<-180)}
#' }
#' The following tests can be selected:
#' \describe{
#'  \item{\code{zero}}{lat and lon are both zero}
#'  \item{\code{equal}}{lat and lon are equal}
#'  \item{\code{countries}}{point is outside the bounds of the country listed}
#'  \item{\code{centroid}}{distance between point and country centroid is less than or equal to the centroid_dis specified}
#' }
#' @return Data.frame containing records from the input data set that passed the coordinate cleaning tests. Number of records removed at each step is printed to the console.
#' @export
#'
#' @examples
#' library(fungarium)
#' data(agaricales) #import sample data set
#' clean <- coord_clean(agaricales) #clean dates
#'

coord_clean <- function(data, lat="decimalLatitude", lon="decimalLongitude", country="country",
                        tests=c("all"),
                        centroid_dis=100,
                        round_digits=4){
  #check that the input is formatted correctly. If not, stop and print error.
  if (!is.data.frame(data)){
    stop('Input data needs to be a data.frame.')
  }

  if(tests=="all"){
    tests <- c("zero", "equal", "countries", "centroids")
  }
  if(!is.null(centroid_dis)){
    centroid_dis <- units::as_units(centroid_dis, "m")
  }
  row0 <- nrow(data)
  row1 <- row0
  if (!is.null(round_digits)){
    data[[lon]] <- round(as.numeric(data[[lon]]), digits = round_digits)
    data[[lat]] <- round(as.numeric(data[[lat]]), digits = round_digits)
    data$x <- data[[lon]]
    data$y <- data[[lat]]
  }else{
    data$x <- as.numeric(data[[lon]])
    data$y <- as.numeric(data[[lat]])
  }

  data <- data[!is.na(data$y)&!is.na(data$x),]
  row2 <- nrow(data)
  message(paste0("'non-numeric coord' test: ", (row1-row2), " records removed."))
  data <- data[data$y<=90&data$y>=-90&data$x<=180&data$x>=-180,]
  message(paste0("'non-valid coord' test: ", (row2-nrow(data)), " records removed."))

  if ("zero" %in% tests){
    row1 <- nrow(data)
    data <- data[data$x!=0&data$y!=0,]
    message(paste0("'zero' test: ", (row1-nrow(data)), " records removed."))
  }
  if ("equal" %in% tests){
    row1 <- nrow(data)
    data$diff <- data$x==data$y
    data <- data[data$diff==F,]
    message(paste0("'equal' test: ", (row1-nrow(data)), " records removed."))
  }
  if (T%in%(c("countries","centroids" ) %in% tests)){
    shp <- rnaturalearth::ne_countries('large', returnclass = "sf")#import world shp file
    data <- sf::st_as_sf(data, coords = c("x", "y"), crs = sf::st_crs(shp)) #convert points to sf points
    shp <- sf::st_transform(shp, crs = "+proj=cea")
    data <- sf::st_transform(data, crs = sf::st_crs(shp))
  }

  if ("countries" %in% tests){
    message("Running 'countries' test...")
    row1 <- nrow(data)
    data <- data[data[[country]]!="",]
    within <- sf::st_intersects(data, shp, prepared=T)
    within <- as.integer(within)
    within<- within-1 #shapefile row labels start at 0
    data$row_numb <- within
    rm(within)
    shp2 <- as.data.frame(shp)
    #shp2 <- subset(shp2,select=-c(geometry))
    shp2 <- shp2[,!colnames(shp2)%in%"geometry"]
    shp2$row_numb <- 0:(nrow(shp2)-1) #shapefile row labels start at 0
    data2 <- as.data.frame(data)
    data2 <- data2[,c("row_numb",country)]
    data2 <- dplyr::left_join(data2, shp2, by="row_numb")
    check <- lapply(as.list(as.data.frame(t(data2))),
                    country_check)

    rm(shp2,data2)
    check <- as.logical(check)
    data <- data[check,]
    message(paste0("'countries' test: ", (row1-nrow(data)), " records removed."))
  }
  if ("centroids" %in% tests){
    message("Running 'centroids' test...")
    row1 <- nrow(data)
    centroids <- sf::st_centroid(shp$geometry)
    centroids <- centroids[data$row_numb+1]#shapefile row labels start at 0
    points <- data$geometry
    check <- sf::st_distance(points,centroids, by_element = T)>centroid_dis
    data <- data[check,]
    message(paste0("'centroids' test: ", (row1-nrow(data)), " records removed."))
  }
  message(paste("Total records removed:", row0-nrow(data)))
  return(data)
}



country_check <- function(x){
  x <- str_clean(x)
  out <- x[2]%in%x[3:length(x)]
  return(out)
}



