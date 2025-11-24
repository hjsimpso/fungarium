#' @title Overlay grid with equal area cells onto map
#'
#' @description
#' Overlays hexagonal grid cells of specified area onto equal area world map.
#'
#' @param data `Data.frame`.
#' @param lat_col Character. description
#' @param lon_col Character. description
#' @param size Integer. Grid cell area (in square kilometers).
#' @param proj Character. Equal area map projection to use. Options: 'cea'
#' (cylindrical equal area) or 'eqearth' (equal earth). Default is 'cea'.
#'
#' @return TODO
#' \describe{
#' \item{\code{geometry}}{Grid cell geometry.}
#' \item{\code{grid_cell_id}}{Grid cell ID. Each grid cell has a unique number.}
#' \item{\code{grid_cell_size}}{The area (in sqauare kilometers) of each grid cell.}
#' }
#' @export
#' @note For more info on the Equal Earth projection see:
#' https://proj.org/en/stable/operations/projections/eqearth.html.
#' For cylindrical equal area see: https://proj.org/en/stable/operations/projections/cea.html.
#' @examples
#' library(fungarium)
#' data(agaricales) #import sample data set
#' clean_geo <- clean_geography(as_dwca(agaricales)) #clean geo
#' grid_data <- assign_grid(clean_geo, 80000L)


assign_grid <- function(data, 
                        lat_col = "lat_parsed",
                        lon_col = "lon_parsed",
                        size, 
                        proj="cea"){
  # check args
  checkmate::assert_data_frame(data)
  checkmate::assert_character(lat_col, max.len = 1)
  checkmate::assert_choice(lat_col, colnames(data))
  checkmate::assert_character(lon_col, max.len = 1)
  checkmate::assert_choice(lon_col, colnames(data))
  checkmate::assert_integer(size, max.len = 1)
  checkmate::assert_character(proj, max.len = 1)
  checkmate::assert_choice(proj, c("cea", "eqearth"))

  # remove records with NA coords
  na_index <- is.na(data[[lat_col]])|is.na(data[[lon_col]])
  if (T%in%na_index){
    warning(paste0(length(na_index[na_index==TRUE]), " records contained NA coordinates and are were removed."))
  }

  # get attributes
  input_attributes <- attributes(data)

  # import world map
  map <- rnaturalearth::ne_countries('large', returnclass = "sf")#import world shp file
  crs_str <- paste0("+proj=", proj, " +ellps=WGS84 +datum=WGS84")
  map <- sf::st_transform(map, crs = crs_str) #transform to cylindrical projection

  # area to cellsize calc
  conv <- 10^6 #km2 to m2
  area <-  size*conv
  side <-  3^(1/4)*sqrt(2*area/9)
  r <-  sqrt(side^2-(side/2)^2)
  grid_size <- r*2

  # make grid
  # TODO allow user to specify offset and select square or hex grid cell type
  grid <- sf::st_as_sf(sf::st_make_grid(map,
                                        cellsize = grid_size,
                                        what = 'polygons',
                                        square = F,
                                        flat_topped = T,
                                        crs=sf::st_crs(map)))

  grid$grid_cell_id <- seq.int(nrow(grid))
  grid$grid_cell_size <- size
  colnames(grid)[colnames(grid)=="x"] <- "geometry"
  sf::st_geometry(grid) <- "geometry"

  # overlay grid onto input data
  data <- sf::st_as_sf(data, coords = c("lon_parsed", "lat_parsed"), crs = "+proj=latlong +ellps=WGS84 +datum=WGS84", remove=F)
  data <- sf::st_transform(data, crs = crs_str) #transform to cylindrical projection
  data <- sf::st_join(grid, data, join=sf::st_contains, left=F) #add hex ID to aga
  data <- sf::st_drop_geometry(data)

  # add attributes
  attributes_to_copy <- input_attributes[!names(input_attributes) %in% c("names", "row.names")]
  attributes(data) <- c(attributes(data)[names(attributes(data)) %in% c("names", "row.names")], attributes_to_copy)

  # output
  out <- list(data=data, grid_ref=grid)
  return(out)
}
