#' Visualize geographic patterns of trait data on a world map
#'
#' Annotate custom geometries on an equal area world map with trait data. World map contains country boundaries.
#'
#' @param data           Data.frame with trait data (e.g., "trait_ratio" from \code{fungarium::enrichment}) and sf geometries (e.g., bounding boxes; see example).
#' @param aes_mapping    Aesthetics mapping for annotating geometries. Default: aes(fill=trait_ratio). In this case, geometries will be filled with color depending on the trait_ratio value. See \code{ggplot2} for customizing color scales.
#' @param country_color  Character string specifying the outline color of countries in the world map. Default: "grey40"
#' @param country_fill   Character string specifying the fill color of countries in the world map. Default: "white"
#' @param country_alpha  Numeric specifying the country fill alpha in the world map. Default: 1
#' @param country_size   Numeric specifying country outline thickness. Default: 0.1
#' @param ...            Additional aesthetics passed to geometries (see example).
#' @return           Returns a gg object.
#' @details Use x and y scale limits to crop map to desired region of interest (see example below).
#' @export
#' @import ggplot2
#' @examples
#' library(fungarium)
#' data("agaricales_updated")
#'
#' #clean coordinates and dates
#' agaricales_cleaned <- coord_clean(agaricales_updated)
#' agaricales_cleaned <- date_clean(agaricales_cleaned)
#'
#' #make hex grid
#' grid <- hex_grid(20000)
#'
#' #convert lat/long points to sf geometry
#' agaricales_sf <- sf::st_as_sf(agaricales_cleaned,
#'                               coords = c("decimalLongitude", "decimalLatitude"),
#'                               crs = "+proj=latlong +ellps=WGS84 +datum=WGS84")
#'
#' #convert to cylindrical equal area projection coordinates
#' agaricales_sf <- sf::st_transform(agaricales_sf,
#'                                   crs = "+proj=cea +ellps=WGS84 +datum=WGS84")
#'
#' #assign points to grid cells
#' agaricales_grid <- sf::st_join(grid, agaricales_sf, join=sf::st_contains, left=FALSE)
#'
#'
#' #find fire-associated records
#' string1 <- "(?i)charred|burn(t|ed)|scorched|fire.?(killed|damaged|scarred)|killed.by.fire"
#' string2 <- "(?i)un.?burn(t|ed)"
#' trait_rec <- find_trait(agaricales_grid, pos_string=string1, neg_string=string2)
#'
#' #aggregate occurrences by year-hex_cell-species (optional)
#' agaricales_grid <- agaricales_grid[,c("year_fixed", "hex", "new_species", "geometry")]
#' agaricales_grid <- dplyr::distinct(as.data.frame(agaricales_grid))
#' trait_rec <- trait_rec[,c("year_fixed", "hex", "new_species", "geometry")]
#' trait_rec <- dplyr::distinct(as.data.frame(trait_rec))
#'
#' #find fire-associated enrichment per grid cell
#' fire_enrich <- enrichment(agaricales_grid, trait_rec, ext_var="geometry", by="hex")
#'
#' #filter out hex cells with low amount of occurrences
#' fire_enrich <- fire_enrich[fire_enrich$freq>=5,]
#'
#' #plot trait data on world map
#' library(ggplot2)
#' fire_enrich <- sf::st_as_sf(fire_enrich) #convert enrichment table back to sf
#' map <- trait_map(fire_enrich, aes_mapping = aes(fill=trait_ratio),
#'                  color=NA, alpha=0.95, size=0.1)
#' map
#'
#' #crop map to area of interest (optional)
#' map+
#'   ylim(c(300000,6000000))+
#'   xlim(c(-18000000,-6000000))


trait_map <- function(data, aes_mapping=aes(fill=trait_ratio), country_color="grey40", country_fill="white",
                      country_alpha=1, country_size=0.1, ...){
  ###import world map
  shp <- rnaturalearth::ne_countries('large', returnclass = "sf")#import world shp file
  shp <- sf::st_transform(shp, crs = "+proj=cea +ellps=WGS84 +datum=WGS84") #transform to cylindrical projection
  #make plot
  ggplot(shp)+
    geom_sf(color=country_color, fill=country_fill, alpha=country_alpha, size=country_size)+
    geom_sf(data=data, mapping=aes_mapping, ..., inherit.aes = F)
}

