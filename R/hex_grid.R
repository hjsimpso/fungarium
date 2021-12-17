#' Overlay grid on map
#'
#' Overlays hexagonal grid cells of specified area onto equal area world map.
#'
#' @param size Integer specifying grid cell area (in square kilometers).
#' @return sf object containing:
#' \describe{
#' \item{\code{geometry}}{grid cell geometry}
#' \item{\code{hex}}{hex number ID. Each grid cell has a unique number}
#' \item{\code{size}}{the area (in sqauare kilometers) of each grid cell}
#' }
#' @export
#' @details World map projection is cylindrical equal area.
#' @examples
#' library(fungarium)
#'
#' #import world shape file
#' shp <- rnaturalearth::ne_countries('large', returnclass = "sf")
#'
#' #transform to cylindrical projection (equal area)
#' shp <- sf::st_transform(shp, crs = "+proj=cea +ellps=WGS84 +datum=WGS84")
#'
#' #get grid
#' grid <- hex_grid(80000)


hex_grid <- function(size){
  ###import world map
  map <- rnaturalearth::ne_countries('large', returnclass = "sf")#import world shp file
  map <- sf::st_transform(map, crs = "+proj=cea +ellps=WGS84 +datum=WGS84") #transform to cylindrical projection
  ##area to cellsize calc
  conv <- 10^6 #km2 to m2
  area <-  size*conv
  side <-  3^(1/4)*sqrt(2*area/9)
  r <-  sqrt(side^2-(side/2)^2)
  grid_size <- r*2

  ##make grid
  grid <- sf::st_as_sf(sf::st_make_grid(map,
                                cellsize = grid_size,
                                what = 'polygons',
                                square = F,
                                flat_topped = T,
                                crs=sf::st_crs(map)))

  grid$hex <- seq.int(nrow(grid))
  grid$size <- size
  colnames(grid)[colnames(grid)=="x"] <- "geometry"
  sf::st_geometry(grid) <- "geometry"
  return(grid)
}
