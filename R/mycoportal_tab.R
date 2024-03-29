#' Retrieve MyCoPortal datasets
#'
#' Enables programmatic interaction with the MyCoPortal web interface to retrieve complete
#' datasets of sporocarp records from fungaria collections and citizen-science observations.
#' Datasets are downloaded as tab-delimited files to a specified directory and then automatically read into R as a data.frame, if desired.
#' This function was modified from the \code{mycoportal} function in the rMyCoPortal package (Krah et al. 2019).
#' See Simpson and Schilling (2021).
#'
#' @param download_dir Character string specifying the path to the directory where you would like files downloaded from MyCoPortal to be stored.
#' @param read_files Logical. If TRUE, files are downloaded into the specified directory AND are automatically imported into R as a data.frame. If FALSE, files are still downloaded into the specified directory, but not imported into R.
#' @param collection Character vector of collection codes specifying which collections will be queried for relevant data (Default is "all", meaning all collections are queried). Use \code{mp_collections} to view all valid collection codes.
#' @param taxon Character string specifying the taxon name (e.g., species name, family name or higher taxon).
#' @param country Character string specifying country, e.g., "USA"
#' @param state Character string specifying state, e.g., "Massachusetts"
#' @param county Character string specifying county, e.g., "Worcester"
#' @param locality Character string specifying locality, e.g., "Harvard Forest"
#' @param elevation_from Character string; meters, e.g., "1000"
#' @param elevation_to Character string; meters
#' @param taxon_type Character, "1" to "5"; (1)Family or Scientific Name, (2)Family Only, (3)Scientific Name only, (4)Higher Taxonomy, (5)Common Name
#' @param north_lat Character string, bounding box coordinate; ("-90" to "90")
#' @param south_lat Character string, bounding box coordinate; ("-90" to "90")
#' @param west_lon Character string, bounding box coordinate; ("-180" to "180")
#' @param east_lon Character string, bounding box coordinate; ("-180" to "180")
#' @param point_lat Character string, point-radius coordinate; ("-90" to "90")
#' @param point_lon Character string, point-radius coordinate; ("-180" to "180")
#' @param radius Character string, point radius; km, e.g., "50"
#' @param collector Character string specifying collector's last name
#' @param collector_num Character string specifying collector's number
#' @param coll_date1 Character string specifying collection data from, e.g., "19 August 1926"
#' @param coll_date2 Character string specifying collection data from, e.g., "19 August 2018"
#' @param synonyms Logical. If TRUE, synonyms from MycoBank and IndexFungorum are searched
#' @param rec_numb Logical. If TRUE, the number of available records is displayed and user is prompted with yes/no question about proceeding with the download. If FASLE, number of records is not displayed and download proceeds automatically.
#' @param messages Logical. If TRUE, updates are printed when navigating to the MyCoPortal website and downloading datasets.
#'
#' @details
#' Docker software must be installed and running on your system before using this function.
#' For additional details on Docker see 'Introduction' vignette via \code{vignette("help", package = "fungarium")}.\cr
#' @note
#' Queries that yield a large number of records may require excessive processing
#' time to download and import. Use \code{rec_numb} parameter if you would like
#' to check for the number of records before proceeding with download (this is the default).
#' Exceptionally large datasets may cause memory (RAM) issues during import into R. This
#' depends on each system and the available memory, but when memory limits are reached
#' during import the resulting data.frame may be truncated; however, the MyCoPortal file will still
#' be stored in the specified download directory. To avoid truncation, try increasing memory
#' availability prior to using \code{mycoportal_tab} or set the \code{read_file} option
#' to FALSE and import the file into R manually (via \code{\link{read.delim}}) at a later time when
#' more memory may be available.\cr
#' \cr
#' Currently, downloads of more than 1,000,000 can be initiated in the MyCoPortal,
#' but during the download process these datasets are ultimately truncated at 1,000,000 records.
#' This is a MyCoPortal server issue. To circumvent this issue, large datasets can be downloaded in
#' smaller batches and then combined later. This is automatically enabled in \code{mycoportal_tab}, by splitting
#' the dataset based on collections (i.e., multiple smaller datasets are downloaded by querying 10 collections at a time).
#' Smaller datasets can then be recombined into one large dataset later (this is a functional solution as of June 13, 2021).
#' In this scenario where a large dataset must be split up, the downloaded files will
#' not be automatically loaded into R,
#' regardless of whether \code{read_files} is TRUE or FALSE. Hence, all downloaded files will need to be manually
#' loaded into R and recombined into one large dataset by the user.
#' @references \enumerate{
#' \item Krah FS, Bates S, Miller A. 2019. rMyCoPortal - an R package to interface
#' with the Mycology Collections Portal. Biodiversity Data Journal 7:e31511.
#' \item Hunter J. Simpson & Jonathan S. Schilling (2021) Using aggregated field
#' collection data and the novel r package fungarium to investigate fungal fire
#' association, Mycologia, 113:4, 842-855, DOI: 10.1080/00275514.2021.1884816
#' }
#' @export
#' @return If read_files is TRUE, a data.frame of MyCoPortal records is returned.
#'
#' @examples
#' library(fungarium)
#' #Query for all Pleurotus records in Minnesota, USA.
#' mp_query <- mycoportal_tab(path.expand("~"), "Pleurotus",
#'                            taxon_type="1", country="United States",
#'                            state="Minnesota", read_files=TRUE,
#'                            messages=FALSE, rec_numb=FALSE)
#' #Query for all Polyporales records in Minnesota, USA.
#' mp_query <- mycoportal_tab(path.expand("~"), "Polyporales",
#'                            taxon_type="4", country="United States",
#'                            state="Minnesota", read_files=TRUE,
#'                            messages=FALSE, rec_numb=FALSE)
#'

mycoportal_tab <- function (download_dir, taxon, country = NULL, state = NULL,
                    county = NULL, locality = NULL, elevation_from = NULL, elevation_to = NULL,
                    taxon_type = "1", north_lat = NULL,
                    south_lat = NULL, west_lon = NULL, east_lon = NULL, point_lat = NULL,
                    point_lon = NULL, radius = NULL, collector = NULL, collector_num = NULL,
                    coll_date1 = NULL, coll_date2 = NULL, synonyms = TRUE, messages = TRUE,
                    rec_numb=TRUE, read_files=TRUE, collection="all")
{
  #check for dependencies
  if (!requireNamespace("RSelenium", quietly = TRUE)) {
    stop("Please install the \"RSelenium\" package.",
         call. = FALSE)
  }

  #check input arguments
  if(T%in%(!collection%in%c("all",mp_collections()$coll_code))){
    stop("One or more collection codes are invalid. Please enter valid collection code or 'all'. See fungarium::mp_collections().")
  }

  if(length(c(taxon, country, state,
              county, locality, elevation_from, elevation_to,
              north_lat,
              south_lat , west_lon, east_lon, point_lat,
              point_lon, radius, collector, collector_num,
              coll_date1, coll_date2))==0){
    stop("Taxon AND location parameters cannot both be NULL. Please enter valid value(s).")
  }
  if("" %in% c(download_dir, taxon, taxon_type, country, state,
       county, locality, elevation_from, elevation_to,
       north_lat,
       south_lat , west_lon, east_lon, point_lat,
       point_lon, radius, collector, collector_num,
       coll_date1, coll_date2)){
    stop('Please do not enter blanks (i.e."") for any input character strings.')
  }

  if(is.null(download_dir) | !dir.exists(download_dir)){
    stop("Please enter a valid download directory.")
  }

  if(!is.character(download_dir)){stop("'download_dir' must be a character string.")}
  if(!is.null(taxon) & !is.character(taxon)){stop("'taxon' must be a character string.")}
  if(!is.null(country) & !is.character(download_dir)){stop("'download_dir' must be a character string.")}
  if(!is.null(state) & !is.character(state)){stop("'state' must be a character string.")}
  if(!is.null(county) & !is.character(county)){stop("'county' must be a character string.")}
  if(!is.null(locality) & !is.character(locality)){stop("'locality' must be a character string.")}
  if(!is.null(elevation_from) & !is.character(elevation_from)){stop("'elevation_from' must be a character string.")}
  if(!is.null(elevation_to) & !is.character(elevation_to)){stop("'elevation_to' must be a character string.")}
  if(!is.null(north_lat)){if(!is.character(north_lat) | !as.numeric(north_lat) %in% c(-90:90)){stop("'north_lat' must be a character string. ('-90' to '90')")}}
  if(!is.null(south_lat)){if(!is.character(south_lat) | !as.numeric(south_lat) %in% c(-90:90)){stop("'south_lat' must be a character string. ('-90' to '90')")}}
  if(!is.null(north_lat) & !is.null(south_lat)){if(as.numeric(north_lat)<as.numeric(south_lat)){stop("'north_lat' must be higher than 'south_lat'")}}
  if(!is.null(west_lon)){if(!is.character(west_lon) | !as.numeric(west_lon) %in% c(-180:180)){stop("'west_lon' must be a character string. ('-180' to '180')")}}
  if(!is.null(east_lon)){if(!is.character(east_lon) | !as.numeric(east_lon) %in% c(-180:180)){stop("'east_lon' must be a character string. ('-180' to '180')")}}
  if(!is.null(east_lon) & !is.null(west_lon)){if(as.numeric(east_lon)<as.numeric(west_lon)){stop("'east_lon' must be higher than 'west_lon'")}}
  if(!is.null(point_lat)){if(!is.character(point_lat) | !as.numeric(point_lat) %in% c(-90:90)){stop("'point_lat' must be a character string. ('-90' to '90')")}}
  if(!is.null(point_lon)){if(!is.character(point_lon) | !as.numeric(point_lon) %in% c(-180:180)){stop("'point_lon' must be a character string. ('-180' to '180')")}}
  if(!is.null(radius) & !is.character(radius)){stop("'radius' must be a character string.")}
  if(!is.null(collector) & !is.character(collector)){stop("'collector' must be a character string.")}
  if(!is.null(collector_num) & !is.character(collector_num)){stop("'collector_num' must be a character string.")}
  if(!is.null(coll_date1) & !is.character(coll_date1)){stop("'coll_date1' must be a character string.")}
  if(!is.null(coll_date2) & !is.character(coll_date2)){stop("'coll_date2' must be a character string.")}
  if(!is.logical(synonyms)){stop("'synonyms' must be a logical.")}
  if(!is.logical(rec_numb)){stop("'rec_numb' must be a logical.")}
  if(!is.logical(read_files)){stop("'read_files' must be a logical.")}
  if(!is.logical(messages)){stop("'messages' must be a logical.")}
  if(is.null(taxon_type)){stop("Please enter a valid value for 'taxon_type'. i.e. character '1' to '5'")}
  if(!is.character(taxon_type) | !as.integer(taxon_type) %in% c(1:5)){stop("Please enter a valid value for 'taxon_type'. i.e. character '1' to '5'")}

  if(length(unique(c(is.null(point_lon), is.null(point_lat), is.null(radius))))>1){
    stop("Please enter values for all lat/long point-radius parameters or keep them all NULL")
  }
  if(length(unique(c(is.null(west_lon), is.null(east_lon), is.null(north_lat), is.null(south_lat))))>1){
    stop("Please enter values for all lat/long bounding box parameters or keep them all NULL")
  }

  #check for internet connection
  if(class(try(curlGetHeaders("r-project.org"),silent = T))=="try-error"){
    stop("Not connected to the internet. Please create a stable connection and try again.")
  }

  #check for memory issues
  if(tryCatch({system2("ls", stdout = T, stderr = F)
    "no_error"},
    error=function(e){"error"},
    warning=function(w){"warning"}) %in% c("error", "warning") &
    tryCatch({system2("cmd", c("/c", "dir"), stdout = F, stderr = F)
      "no_error"},
      error=function(e){"error"},
      warning=function(w){"warning"}) %in% c("error", "warning")){
    stop("Insufficient memory available.")
  }

  #check if Docker is installed and running
  if(tryCatch({system2(command = "docker", args = c("-v"), stdout = T, stderr = F)
    "no_error"},
    error=function(e){"error"},
    warning=function(w){"warning"}) %in% c("error", "warning")){
    stop("Docker not installed. Please install Docker")
  }

  if(tryCatch({system2(command = "docker", args = c("ps"), stdout = T, stderr = F)
    "no_error"},
    error=function(e){"error"},
    warning=function(w){"warning"}) %in% c("error", "warning")){
    stop("Docker is not currently running on your system. Please start Docker.")
  }

  #check if selenium Docker image is already installed; if not, use "docker pull" to install image
  if(tryCatch({system2("docker", args=c("inspect", "--format=\"{{.ID}}\"", "selenium/standalone-chrome:latest"),stdout = T, stderr = F)
    "no_error"},
    error=function(e){"error"},
    warning=function(w){"warning"}) %in% c("error", "warning")){
    if(messages){message('"selenium-chrome" is not installed. Installing now...\n')}
    system2("docker", c("pull", "selenium/standalone-chrome:latest"))
  }else{
    #check if selenium should be updated (if image is older than 90 days)
    if(as.numeric(Sys.Date() - as.Date(substring(system2("docker", c("inspect", "--format=\"{{.Created}}\"", "selenium/standalone-chrome:latest"), stdout = T, stderr = F), first=1, last=10)))>90){
      if(messages){message('Updating "selenium-chrome"...\n')}
      system2("docker", c("pull", "selenium/standalone-chrome:latest"))
    }
  }

  #check if container by same name already exists; if so, stop and remove container
  if(!tryCatch({system2("docker", c("inspect", "--format=\"{{.Name}}\"", "sel_con"),stdout = T, stderr = F)
   "no_error"},
    error=function(e){"error"},
    warning=function(w){"warning"}) %in% c("error", "warning")){
    system2("docker", c("rm", "-f", "sel_con"), stdout = F, stderr = F)
  }

  #Set on.exit parameter to stop and remove container
  on.exit(system2("docker", c("rm", "-f", "sel_con"), stdout = F, stderr = F), add = T)

  #start selenium container
  if(messages){message("Attempting to start Selenium container...\n")}
  if(tryCatch({system2("docker", args=c("run", "-d", "--name", "sel_con", "-p", "4445:4444", "--shm-size", "2g", "selenium/standalone-chrome:latest"),stdout = T, stderr = F)
    "no_error"},
    error=function(e){"error"},
   warning=function(w){"warning"}) %in% c("error", "warning")){
    stop('Error. Unable to start Selenium container. Check that port 4445 is not already allocated.')
  }else{
    if(messages){message("Selenium container successfully started.\n")}
  }

  #Delay until container is fully running
  while(tryCatch({system2("docker", args=c("inspect", "--format=\"{{.Name}}\"", "sel_con"),stdout = T, stderr = F)
    "no_error"},
    error=function(e){"error"},
    warning=function(w){"warning"}) %in% c("error", "warning")){
    Sys.sleep(2)
  }

  #Set up remoteDriver
  dr <- RSelenium::remoteDriver(port = 4445L, browserName = "chrome")
  while(tryCatch({dr$open(silent=T)
    "no_error"},
    error=function(e){"error"},
    warning=function(w){"warning"}) %in% c("error", "warning")){
    Sys.sleep(2)
  }

  #go to mycoportal website
  if(messages){message("Navigating to mycoportal website...\n")}
  dr$navigate("https://mycoportal.org/portal/collections/index.php")
  dr$screenshot(display = T)
  k <- 1
  while(tryCatch({webElem <- dr$findElement(using = "xpath", value='//*[@id="specobsdiv"]/form/div[2]/div[1]/button')#check if webpage can load successfully
  "no_error"},
  error=function(e){"error"},
  warning=function(w){"warning"},
  message=function(m){"message"}) %in% c("error", "warning", "message") & k<10){
    Sys.sleep(2)
    k <- k + 1
  }
  if(k>=10){stop("MyCoPortal website currently unavailable.")}

  #make collection selection
  if(T%in%(collection%in%"all")){
    webElem$clickElement()
  }else{#select collections
    webElem <- dr$findElement(using = "xpath", value='//*[@id="dballcb"]')#deselect "all collections" box
    webElem$clickElement()
    mp_colls <- mp_collections()
    coll_order <- mp_colls[mp_colls$coll_code%in%collection,]$order
    webElem <- dr$findElements(using = "name", value = "db[]")
    webElem <- webElem[coll_order+1]
    for (i in 1:length(webElem)){
      webElem[[i]]$clickElement()
    }
    #submit collection selection
    webElem <- dr$findElement(using = "xpath", '//*[@id="specobsdiv"]/form/div[2]/div[1]/button')
    webElem$clickElement()
  }
  dr$screenshot(display = T)


  #enter query parameters
  webElem <- dr$findElement(using = "xpath", '//*[@id="harvestparams"]/div[1]/div[2]/div[3]/button')#reset query form
  webElem$clickElement()
  dr$screenshot(display = T)
  if(synonyms==FALSE){#uncheck synonym box
    webElem <- dr$findElement(using = "xpath", value='//*[@id="harvestparams"]/div[1]/div[1]/div[1]/div[2]/input')
    webElem$clickElement()
  }
  webElem <- dr$findElement(using = "xpath", paste("//*[@id='taxontype']/option[",
                                                    taxon_type, "]"))
  webElem$clickElement()
  dr$screenshot(display = T)
  if(messages){message("Entering query parameters...\n")}
  if(!is.null(taxon)){
    #webElem <- dr$findElement("id", "taxa")
    webElem <- dr$findElement(using = "xpath", value='//*[@id="taxa"]')
    dr$screenshot(display = T)
    webElem$sendKeysToElement(list(taxon))}
  if(!is.null(country)){
    webElem <- dr$findElement("id", "country")
    webElem$sendKeysToElement(list(country))}
  if(!is.null(state)){
    webElem <- dr$findElement("id", "state")
    webElem$sendKeysToElement(list(state))}
  if(!is.null(county)){
    webElem <- dr$findElement("id", "county")
    webElem$sendKeysToElement(list(county))}
  if(!is.null(locality)){
    webElem <- dr$findElement("id", "locality")
    webElem$sendKeysToElement(list(locality))}
  if(!is.null(elevation_from)){
    webElem <- dr$findElement("id", "elevlow")
    webElem$sendKeysToElement(list(elevation_from))}
  if(!is.null(elevation_to)){
    webElem <- dr$findElement("id", "elevhigh")
    webElem$sendKeysToElement(list(elevation_to))}
  if(!is.null(north_lat)){
    if(length(grep("-", north_lat))>0){
      webElem <- dr$findElement(using = "xpath", '//*[@id="nlS"]')
      webElem$clickElement()
    }else{
      webElem <- dr$findElement(using = "xpath", '//*[@id="nlN"]')
      webElem$clickElement()
    }
    webElem <- dr$findElement("id", "upperlat")
    webElem$sendKeysToElement(list(north_lat))
    }
  if(!is.null(south_lat)){
    if(length(grep("-", south_lat))>0){
      webElem <- dr$findElement(using = "xpath", '//*[@id="blS"]')
      webElem$clickElement()
    }else{
      webElem <- dr$findElement(using = "xpath", '//*[@id="blN"]')
      webElem$clickElement()
    }
    webElem <- dr$findElement("id", "bottomlat")
    webElem$sendKeysToElement(list(south_lat))
    }
  if(!is.null(west_lon)){
    if(length(grep("-", west_lon))>0){
      webElem <- dr$findElement(using = "xpath", '//*[@id="llW"]')
      webElem$clickElement()
    }else{
      webElem <- dr$findElement(using = "xpath", '//*[@id="llE"]')
      webElem$clickElement()
    }
    webElem <- dr$findElement("id", "leftlong")
    webElem$sendKeysToElement(list(west_lon))
    }
  if(!is.null(east_lon)){
    if(length(grep("-", east_lon))>0){
      webElem <- dr$findElement(using = "xpath", '//*[@id="rlW"]')
      webElem$clickElement()
    }else{
      webElem <- dr$findElement(using = "xpath", '//*[@id="rlE"]')
      webElem$clickElement()
    }
    webElem <- dr$findElement("id", "rightlong")
    webElem$sendKeysToElement(list(east_lon))
    }
  if(!is.null(point_lat)){
    if(length(grep("-", point_lat))>0){
      webElem <- dr$findElement(using = "xpath", '//*[@id="S"]')
      webElem$clickElement()
    }else{
      webElem <- dr$findElement(using = "xpath", '//*[@id="N"]')
      webElem$clickElement()
    }
    webElem <- dr$findElement("id", "pointlat")
    webElem$sendKeysToElement(list(point_lat))}
  if(!is.null(point_lon)){
    if(length(grep("-", point_lon))>0){
      webElem <- dr$findElement(using = "xpath", '//*[@id="W"]')
      webElem$clickElement()
    }else{
      webElem <- dr$findElement(using = "xpath", '//*[@id="E"]')
      webElem$clickElement()
    }
    webElem <- dr$findElement("id", "pointlong")
    webElem$sendKeysToElement(list(point_lon))}
  if(!is.null(radius)){
    webElem <- dr$findElement("id", "radiustemp")
    webElem$sendKeysToElement(list(radius))}
  if(!is.null(collector)){
    webElem <- dr$findElement("id", "collector")
    webElem$sendKeysToElement(list(collector))}
  if(!is.null(collector_num)){
    webElem <- dr$findElement("id", "collnum")
    webElem$sendKeysToElement(list(collector_num))}
  if(!is.null(coll_date1)){
    webElem <- dr$findElement("id", "eventdate1")
    webElem$sendKeysToElement(list(coll_date1))}
  if(!is.null(coll_date2)){
    webElem <- dr$findElement("id", "eventdate2")
    webElem$sendKeysToElement(list(coll_date2))}
  if(messages){message("Submitting query...\n")}
  webElem$sendKeysToElement(list(key = "enter"))
  while(tryCatch({webElem <- dr$findElement(using = "xpath", value = '//*[@id="queryrecords"]/div[5]/div[3]')
  "no_error"},
  error=function(e){"error"},
  warning=function(w){"warning"},
  message=function(m){"message"}) %in% c("error", "warning", "message")){
    Sys.sleep(2)
  }
  elemText <- webElem$getElementText()[[1]]
  no_recs <- grep("\\s0$|of$", elemText, perl = TRUE, value = TRUE)
  if (length(no_recs) > 0) {
    stop(paste("Query error:", " no records found for your query", ". ", Sys.time(), sep = ""))
  } else {
    recs <- gsub("(.*\\s)(\\S+$)", "\\2", elemText)
    if(rec_numb==T){
      ask <- readline(prompt=paste(recs, " records available. Proceed with download (yes/no): ", sep=""))
      while(!ask %in% c("no","n","yes","y")){
        ask <- readline(prompt="Please enter 'yes' or 'no': ")
      }
      if(ask %in% c("no", "n")){
        stop("Download cancelled by user.")
      }
    }

    webElem <- dr$findElement(using = 'xpath', value = '//*[@id="queryrecords"]/div[1]/form[2]/button')
    webElem$clickElement()
    #navigate to download pop-up window
    myswitch(dr,dr$getWindowHandles()[[2]])
    #deselect zip file option
    while(tryCatch({webElem <- dr$findElement("name", "zip")
    "no_error"},
    error=function(e){"error"},
    warning=function(w){"warning"},
    message=function(m){"message"}) %in% c("error", "warning", "message")){
      Sys.sleep(2)
    }
    webElem$clickElement()

    #select tab file option
    webElem <- dr$findElement(using = "xpath", value = '/html/body/div[1]/div[2]/form/fieldset/div[3]/div[2]/input[2]')
    webElem$clickElement()
    dload_url <- dr$getCurrentUrl()[[1]]
    webElem <- dr$findElement("name", "submitaction")
    if(messages){message("Attempting download...\n")}
    webElem$setTimeout(type = "page load", milliseconds = 10000000)

    #download file
    webElem$clickElement()
    dload_post_url <- dr$getCurrentUrl()[[1]]
    if(dload_post_url != dload_url){
      stop(paste("Download error: ", "file too large. ", Sys.time(), "\n", sep = ""))
    }
  }

  #Transfer file from selenium container to download_dir
  tryCatch(tab_file <- system2("docker", c("exec", "sel_con", "ls", "/home/seluser/Downloads"), stdout = T))
  while(length(grep(".+\\.tab$",tab_file))==0){
    Sys.sleep(1+(as.numeric(recs)/10000))
    tab_file <- system2("docker", c("exec", "sel_con", "ls", "/home/seluser/Downloads"), stdout = T)
  }
  system2("docker", c("cp", paste("sel_con:/home/seluser/Downloads/", tab_file, sep=""), download_dir), stdout = F, stderr = F)
  if(messages){message(paste("File ", tab_file ," successfully downloaded. ", Sys.time(), "\n", sep = ""))}

  #Read file into R
  if(read_files ==T){
    system2("docker", c("rm", "-f", "sel_con"), stdout = F, stderr = F)
    on.exit()
    if(messages){message("Reading file into R...")}
    files <- utils::read.delim(paste(download_dir,"/", tab_file, sep=""), colClasses = "character", quote="", encoding = "latin1")
    if(nrow(files)!=as.integer(recs)){warning("Data.frame truncated due to memory constraints. Full MyCoPortal file can still be found in the specified download directory.")}
    return(files)

  }
}


