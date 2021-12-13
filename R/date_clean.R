#' Clean dates in occurrence data sets
#'
#' Parses dates with varying format into a consistent format of specified resolution (e.g., year, month, day) and removes problematic dates that cannot be parsed.
#'
#'
#' @param data Data.frame of occurrence data set (e.g. MyCoPortal, GBIF)
#' @param date_col Character string specifying the date column. Default is "eventDate" (i.e., the Darwin Core standard).
#' @param date_res Character string specifying the desired resolution for parsed dates (i.e., "year", "month", "day"). Default is "year".
#'
#' @return Data.frame containing the input data set with the following output fields appended.
#' Records with dates that could not be parsed are removed.
#' \describe{
#' \item{\code{date_fixed}}{Variabe date formats are transformed into standard YYYY-MM-DD format. Dates limited to year or month resolution (e.g., 1990-00-00 or 1990-01-00), are transformed to YYYY or YYYY-MM format respectively.}
#' \item{\code{parsed_date}}{"date_fixed" values are parsed into standard POSIXct date-time objects using \code{lubridate::parse_date_time}}
#' \item{\code{parsed_format}}{Detected date format (e.g., "Y", "Ym", or "Ymd"). Used for parsing dates correctly.}
#' \item{\code{year_fixed}}{The collection year value after cleaning and parsing dates.}
#' \item{\code{month_fixed}}{The collection month value after cleaning and parsing dates.}
#' \item{\code{day_fixed}}{The collection day value after cleaning and parsing dates.}
#' }
#' @note Full dates other than the "Ymd" format are all assumed to be in the "dmY" format, not "mdY". Ex: 01/10/1990 is assumed to be October 10, 1990, not January 10, 1990.
#' @export
#'
#' @examples
#' library(fungarium)
#' data(agaricales) #import sample data set
#' clean <- date_clean(agaricales) #clean dates
#'


date_clean <- function(data, date_col="eventDate", date_res="year"){
  if (date_res=="year"){
    date_res <- "Y"
  }else if(date_res=="month"){
    date_res <- "m"
  }else if(date_res=="day"){
    date_res <- "d"
  }

  months <- data.frame(word=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                       abbr=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                       number=c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
  data$date_fixed <- gsub("^(-|(0|00|0000)(/|\\.|-)(0|00)(/|\\.|-)(0|00|0000))$", "", data[[date_col]])#remove null dates
  data$parsed_format <- ifelse(data$date_fixed=="", "null", "")
  data$date_fixed <- gsub("^([0-9]+)(\\.|/)([0-9]+)(\\.|/)([0-9].*)", "\\1-\\3-\\5", data$date_fixed)#fix sep character
  data$date_fixed <- gsub("^([0-9][0-9][0-9][0-9])-(00|0)-([0-9][0-9]|[0-9])(\\s|/|T|$)", "\\1\\4", data$date_fixed)#remove 00 months (Ymd)
  data$date_fixed <- gsub("^([0-9][0-9]|[0-9])-(00|0)-([0-9][0-9][0-9][0-9])", "\\3", data$date_fixed)#remove 00 month (dmY)
  data$date_fixed <- gsub("^([0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9]))-(00|0)(\\s|/|T|$)", "\\1", data$date_fixed)#remove 00 day
  data$date_fixed <- gsub("^(00|0)-([0-9][0-9]|[0-9])-([0-9][0-9][0-9][0-9])", "\\2-\\3", data$date_fixed)#remove 00 day (dmY)
  data$date_fixed <- gsub("^([0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(|(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]).*)/\\1", "\\1", data$date_fixed)#fix double dates
  data$date_fixed <- gsub("(T|\\s)00:00:00(|\\.0+)", "", data$date_fixed)#fix null time

  data[grep("^[0-9][0-9][0-9][0-9]$", data$date_fixed),"parsed_format"] <- "Y"
  data[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])$", data$date_fixed),"parsed_format"] <- "Ym"
  data[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])$", data$date_fixed),"parsed_format"] <- "Ymd"
  data[grep("^[0-9][0-9][0-9][0-9]-(1[3-9]|[2-9][0-9])($|-)", data$date_fixed),"parsed_format"] <- "bad_month" #for Ymd or Ym
  data[grep("^([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])-[0-9][0-9][0-9][0-9]$", data$date_fixed),"parsed_format"] <- "dmY"
  data[grep(tolower(paste(paste0("([0-9][0-9]|[0-9])\\s", months$word), paste0("([0-9][0-9]|[0-9])\\s", months$abbr), collapse="|", sep="|")), tolower(data$date_fixed)),"parsed_format"] <- "dmY" #dmonthY - for month words
  data[grep("^([0-9][0-9]|[0-9])-(1[3-9]|[2-9][0-9])-[0-9][0-9][0-9][0-9]", data$date_fixed),"parsed_format"] <- "bad_month" #for dmY
  data[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]", data$date_fixed),"parsed_format"] <- "YmdT"
  data[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9](\\+|-)", data$date_fixed),"parsed_format"] <- "YmdTz"
  data[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])((T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]|)/[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])((T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]|)", data$date_fixed),"parsed_format"] <- "double_date" #when double dates aren't equal
  rows1 <- nrow(data)
  data <- data[data$parsed_format!="null",]
  null_rows <- rows1-nrow(data)
  data <- data[!data$parsed_format%in%c("", "bad_month", "double_date"),]
  error1_rows <- rows1-null_rows-nrow(data)
  data <- data[grep(date_res, data$parsed_format),]
  res_rows <- rows1-null_rows-error1_rows-nrow(data)
  data$parsed_date <- lubridate::parse_date_time(data$date_fixed, unique(data$parsed_format))
  data <- data[!is.na(data$parsed_date),]
  error2_rows <- rows1-null_rows-error1_rows-res_rows-nrow(data)
  if(date_res=="Y"){
    data$year_fixed <- lubridate::year(data$parsed_date)
  }else if(date_res=="m"){
    data$year_fixed <- lubridate::year(data$parsed_date)
    data$month_fixed <- lubridate::month(data$parsed_date)
  }else if(date_res=="d"){
    data$year_fixed <- lubridate::year(data$parsed_date)
    data$month_fixed <- lubridate::month(data$parsed_date)
    data$day_fixed <- lubridate::day(data$parsed_date)
  }
  print(paste0("null_rows=", null_rows,"; error1_rows=", error1_rows,"; res_rows=",res_rows, "; error2_rows=", error2_rows, "; total_removed=", rows1-nrow(data)))
  return(data)
}
