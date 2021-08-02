#' Clean lat/long coordinates in fungal collections data sets
#'
#' Parses variably formatted dates into a consistent format of specified resolution (e.g., year, month, day) and removes problematic dates that cannot be parsed.
#'
#'
#' @param data Data.frame of fungal collections data set (e.g. MyCoPortal, GBIF)
#' @param lat Character string specifying the decimal latitude column. Default is "decimalLatitude" (i.e., the Darwin Core standard).
#' @param lon Character string specifying the decimal longitude column. Default is "decimalLongitude" (i.e., the Darwin Core standard).
#' @param tests Character vector specifying the coordinates cleaning tests to perform (i.e., "zero", "equal", "countries", "centroids"). Default is c("zero", "equal", "countries", "centroids").
#'
#' @return Data.frame containing the input data set with the following output fields appended.
#' Records with dates that could not be parsed are removed.
#' \item{date_fixed}{NULL}
#' \item{parsed_date}{NULL}
#' \item{parsed_format}{NULL}
#' \item{year_fixed}{NULL}
#' \item{month_fixed}{NULL}
#' \item{day_fixed}{NULL}
#' @note Full dates other than the "Ymd" format are all assumed to be in the "dmY" format, not "mdY". Ex: 01/10/1990 is assumed to be October 10, 1990, not January 10, 1990.
#' @export
#'
#' @examples
#' library(fungarium)
#' data(strophariaceae) #import sample data set
#' data <- date_clean(data) #clean dates
#'

coord_clean <- function(data, lat="decimalLatitude", lon="decimalLongitude", country="country",
                        tests=c("zero", "equal", "countries", "centroids", "capitals"),
                        centroid_dis=100,
                        show_status=T, round_digits=4){
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
    shp2 <- subset(shp2,select=-c(geometry))
    shp2$row_numb <- 0:(nrow(shp2)-1) #shapefile row labels start at 0
    data2 <- subset(as.data.frame(data), select=c(row_numb,country))
    data2 <- dplyr::left_join(data2, shp2, by="row_numb")
    check <- maxjobs.mclapply.clean_coord(as.list(as.data.frame(t(data2))),
                                          country_check, show_status=show_status,
                                          cores=1)
    rm(shp2)
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
  x <- fungarium:::str_clean(x)
  out <- x[2]%in%x[3:length(x)]
  return(out)
}

#parallelization loop
maxjobs.mclapply.clean_coord <- function(X, FUN, cores, show_status){
  N <- length(X)
  i.list <- parallel::splitIndices(N, N/cores)
  result.list <- list()
  for(i in seq_along(i.list)){
    i.vec <- i.list[[i]]
    try_error <- TRUE
    while (try_error){
      result.list[i.vec] <- parallel::mclapply(X[i.vec], FUN,
                                               mc.cores=cores)
      error_check <- lapply(result.list[i.vec], class)
      if ("try-error" %in% error_check){
        message("'try-error' detected in mclapply output. Rerunning scheduled tasks.")
        try_error <- TRUE
      }else{
        try_error <- FALSE
      }
    }
    #print status message
    if (show_status){
      if((length(X)-(i*cores))>0){
        cat(paste0(round((i*cores / length(X)) * 100), '% completed.',' Records left:', (length(X)-(i*cores)), "   "), "\r") #track progress
      }else{
        cat(paste0('100% completed.',' Records left:0', "   "), "\r") #track progress
      }
    }
  }
  return(result.list)
}



