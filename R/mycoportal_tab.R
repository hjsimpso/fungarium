#' Retrieve MyCoPortal datasets
#'
#' Enables programmatic interaction with the MyCoPortal web interface to retrieve complete
#' datasets of sporocarp records from fungaria collections and citizen-science observations.
#' This function was modified from the \code{mycoportal} function in the rMyCoPortal package (Krah et al. 2019).
#' See Simpson and Schilling (2020).
#'
#'
#' @param taxon Character string specifying the taxon name (e.g., species name, family name or higher taxon). To query higher taxa, e.g. on order level, I recommend using \code{\link{mycoportal_hightax}}
#' @param country Character string specifying country, e.g., "USA"
#' @param state Character string specifying state, e.g., "Massachusetts"
#' @param county Character string specifying county, e.g., "Worcester"
#' @param locality Character string specifying locality, e.g., "Harvard Forest"
#' @param elevation_from Character string, meter, e.g., "1000"
#' @param elevation_to Character string, meter
#' @param host Character string specifying host species, e.g., "Betula alba"
#' @param collection Either "all" or a vector or integers with number corresponding to collection. For a list of collections use function \code{getCollections()}
#' @param taxon_type Integer, one of 1 to 5 representing "Family or Scientific Name", "Scientific Name only", "Family Only", "Higher Taxonomy", Common Name"
#' @param north_lat Character string, coordinate e.g., "45"
#' @param south_lat Character string, coordinate
#' @param west_lon Character string, coordinate, e.g., "-72"
#' @param east_lon Character string, coordinate
#' @param point_lat Character string, coordinate
#' @param point_lon Character string, coordinate
#' @param radius Character string, km, e.g., "50"
#' @param collector Character string specifying collector name
#' @param collector_num Character string specifying collector number
#' @param coll_date1 Character string specifying collection data from, e.g., "19 August 1926"
#' @param coll_date2 Character string specifying collection data from, e.g., "19 August 2018"
#' @param syns Logical, if TRUE synonyms from MycoBank and IndexFungorum are searched
#' @param port Default is 4445L
#' @param verbose Logical
#' @param browserName Character string specifying the browser to use, recommended: "chrome"
#'
#' @return           #Returns a dataframe based of the resulting queried records.
#' @references \enumerate{
#' \item Krah FS, Bates S, Miller A. 2019. rMyCoPortal - an R package to interface
#' with the Mycology Collections Portal. Biodiversity Data Journal 7:e31511.
#' \item Simpson, H.J., Schilling, J.S. 2020. Using aggregated field collections data
#' and the novel R package fungarium to investigate fungal fire association. \emph{Mycologia}.
#' }
#' @export
#'
#' @examples
#' #Query for all Pleurotus records in Minnesota, USA.
#' mp_query <- mycoportal_tab("Pleurotus", country="United States", state="Minnesota")
#'
mycoportal_tab <- function (taxon, country = "", state = "",
                    county = "", locality = "", elevation_from = "", elevation_to = "",
                    host = "", taxon_type = "1", north_lat = "",
                    south_lat = "", west_lon = "", east_lon = "", point_lat = "",
                    point_lon = "", radius = "", collector = "", collector_num = "",
                    coll_date1 = "", coll_date2 = "", verbose = TRUE,
                    port = 4445L, browserName = "chrome")
{
  #check for dependencies
  if (!requireNamespace("rSelenium", quietly = TRUE)) {
    stop("Package \"rSelenium\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  print("Navigating to mycoportal website...")
  dr$navigate("http://mycoportal.org/portal/collections/harvestparams.php")
  Sys.sleep(4)
  webElem <- dr$findElement("class name", "resetbtn")
  webElem$clickElement()
  Sys.sleep(4)
  webElem <- dr$findElement(using = "xpath", paste("//*[@id='taxontype']/option[",
                                                    taxon_type, "]"))
  webElem$clickElement()
  Sys.sleep(4)
  print("Entering query parameters...")
  webElem <- dr$findElement("id", "taxa")
  webElem$sendKeysToElement(list(taxon))
  webElem <- dr$findElement("id", "country")
  webElem$sendKeysToElement(list(country))
  webElem <- dr$findElement("id", "state")
  webElem$sendKeysToElement(list(state))
  webElem <- dr$findElement("id", "county")
  webElem$sendKeysToElement(list(county))
  webElem <- dr$findElement("id", "locality")
  webElem$sendKeysToElement(list(locality))
  webElem <- dr$findElement("id", "elevlow")
  webElem$sendKeysToElement(list(elevation_from))
  webElem <- dr$findElement("id", "elevhigh")
  webElem$sendKeysToElement(list(elevation_to))
  webElem <- dr$findElement("id", "assochost")
  webElem$sendKeysToElement(list(host))
  webElem <- dr$findElement("id", "upperlat")
  webElem$sendKeysToElement(list(north_lat))
  webElem <- dr$findElement("id", "bottomlat")
  webElem$sendKeysToElement(list(south_lat))
  webElem <- dr$findElement("id", "leftlong")
  webElem$sendKeysToElement(list(west_lon))
  webElem <- dr$findElement("id", "rightlong")
  webElem$sendKeysToElement(list(east_lon))
  webElem <- dr$findElement("id", "pointlat")
  webElem$sendKeysToElement(list(point_lat))
  webElem <- dr$findElement("id", "pointlong")
  webElem$sendKeysToElement(list(point_lon))
  webElem <- dr$findElement("id", "radiustemp")
  webElem$sendKeysToElement(list(radius))
  webElem <- dr$findElement("id", "collector")
  webElem$sendKeysToElement(list(collector))
  webElem <- dr$findElement("id", "collnum")
  webElem$sendKeysToElement(list(collector_num))
  webElem <- dr$findElement("id", "eventdate1")
  webElem$sendKeysToElement(list(coll_date1))
  webElem <- dr$findElement("id", "eventdate2")
  webElem$sendKeysToElement(list(coll_date2))
  print("Submitting query...")
  webElem$sendKeysToElement(list(key = "enter"))
  Sys.sleep(4)
  webElem <- dr$findElement(using = "xpath", value = '//*[@id="queryrecords"]/div[5]/div[3]')
  elemText <- webElem$getElementText()[[1]]
  no_recs <- grep("\\s0$|of$", elemText, perl = TRUE, value = TRUE)
  if (length(no_recs) > 0) {
    dload_mess <- paste("Query error:", " no records found for ", taxon, ". ", Sys.time(), sep = "")
    print(dload_mess)
  } else {
    webElem <- dr$findElement(using = "xpath", value = "/html/body/table/tbody/tr[2]/td/div[2]/div/div[2]/div/div[1]/div/a")
    webElem$clickElement()
    Sys.sleep(4)
    webElem <- dr$findElement("name", "zip")
    webElem$clickElement()
    webElem <- dr$findElement(using = "xpath", value = '//*[@id="innertext"]/div[2]/form/fieldset/table/tbody/tr[3]/td[2]/div/input[2]')
    webElem$clickElement()
    dload_url <- dr$getCurrentUrl()[[1]]
    webElem <- dr$findElement("name", "submitaction")
    print("Attempting download...")
    webElem$clickElement()
    dload_post_url <- dr$getCurrentUrl()[[1]]
    if (dload_post_url == dload_url) {
      dload_mess <- paste(taxon, "file successfully downloaded.", Sys.time(), sep = " ")
      print(dload_mess)
    } else {
      dload_mess <- paste("Download error:", taxon, "file too large.", Sys.time(), sep = " ")
      print(dload_mess)
    }
  }
  if ("dload_log" %in% ls(envir = .GlobalEnv)) {
      get("dload_log", envir = .GlobalEnv)
      dload_log <- append(dload_log, dload_mess)
  } else {
    dload_log <- as.vector(dload_mess)
  }
  assign("dload_log", dload_log, envir = .GlobalEnv)
  Sys.sleep(4)
}
