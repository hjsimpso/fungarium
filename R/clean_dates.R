#' Clean dates in occurrence data sets
#'
#' Parses dates with varying format into a consistent format of specified resolution
#' (e.g., year, month, day) and removes problematic dates that cannot be parsed or
#' do not match the specified resolution.
#'
#'
#' @param date_col Character. Dates of varying format.
#'
#' @return Data.frame containing the input data with the following output fields appended.
#' Records with dates that could not be parsed or do not match the specified resolution are removed.
#' \describe{
#' \item{\code{date_harmonized}}{Variabe date formats are transformed into standard YYYY-MM-DD format. Dates limited to year or month resolution (e.g., 1990-00-00 or 1990-01-00), are transformed to YYYY (e.g., 1990) or YYYY-MM (e.g., 1990-01) format respectively.}
#' \item{\code{parsed_format}}{Detected date format (e.g., "Y", "Ym", or "Ymd").}
#' \item{\code{date_parsed}}{"date_harmonized" values are parsed into standard POSIXct date-time objects using \code{lubridate::parse_date_time}}
#' \item{\code{year_parsed}}{The collection year value after cleaning and parsing dates.}
#' \item{\code{month_parsed}}{The collection month value after cleaning and parsing dates.}
#' \item{\code{day_parsed}}{The collection day value after cleaning and parsing dates.}
#' }
#' @note Full dates assumed to be "Ymd" or "dmY"
#' format, not "Ydm", or "mdY". Ex: 01/10/1990 is assumed to be October 10,
#' 1990, not January 10, 1990, and "2020-03-12" is assumed to be March 12, 2020,
#' not December 3, 2020.
#' @export
#'
#' @examples
#' library(fungarium)
#' data(agaricales_updated) #import sample data set
#' clean <- date_clean(agaricales_updated) #clean dates
#'


clean_dates <- function(dates){
  # check args
  checkmate::assert_character(dates)
  checkmate::assert_true(length(dates)>0)

  # reference data
  # months <- data.frame(word=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  #                      abbr=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  #                      number=c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
  #
  # months_str <- paste(paste0(months$word, collapse = "|"), paste0(months$abbr, collapse = "|"), sep = "|")
  # datetime_formats <- data.frame(format = c("Y", "Ym", "mY", "Ymd", "dmY", "dmonthY", "monthdY", "Ymonthd", "Ydmonth"),
  #                                regex = c("^[0-9][0-9][0-9][0-9]$",
  #                                          "^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])$",
  #                                   "^([0-9][0-9]|[0-9])-[0-9][0-9][0-9][0-9]$"))


  Y_fmt <- "[0-9]{4}"
  m_fmt <- "[0-9]{1,2}"
  d_fmt <- "[0-9]{1,2}"
  T_fmt <- "[T\\s][0-9]{2}:[0-9]{2}:[0-9]{2}"
  z_fmt <- "(?i)z|[\\+-][0-9]{2}:?[0-9]{2}"
  month_fmt <- "Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?"

  datetime_formats <- c("Y" = sprintf("^%s$", Y_fmt),
                        "Ym" = sprintf("^%s-%s$", Y_fmt, m_fmt),
                        "mY" = sprintf("^%s-%s$", m_fmt, Y_fmt),
                        "Ymd" = sprintf("^%s-%s-%s$", Y_fmt, m_fmt, d_fmt),
                        "dmY" = sprintf("^%s-%s-%s$", d_fmt, m_fmt, Y_fmt),
                        "dmonthY" = sprintf("^%s-%s-%s$", d_fmt, month_fmt, Y_fmt),
                        "monthdY" = sprintf("^%s-%s-%s$", month_fmt, d_fmt, Y_fmt),
                        "Ymonthd" = sprintf("^%s-%s-%s$", Y_fmt, month_fmt, d_fmt),
                        "Ydmonth" = sprintf("^%s-%s-%s$", Y_fmt, d_fmt, month_fmt),
                        "YmdT" = sprintf("^%s-%s-%s%s$", Y_fmt, month_fmt, d_fmt, T_fmt),
                        "YmdTz" = sprintf("^%s-%s-%s%s%s$", Y_fmt, month_fmt, d_fmt, T_fmt, z_fmt)
  )

  # create result data.frame
  out <- data.frame(date_raw = dates)

  # clean dates
  ## remove special chars
  out$date_cleaned <- gsub("\\.|/|\\\\", " ", out$date_raw)

  ## remove leading or trailing white space
  out$date_cleaned <- gsub("^\\s+|\\s+$", "", out$date_cleaned)

  ## harmonize separator
  sep_regex <- sprintf("^(?:[0-9]+|(?:%s))\\s(?:[0-9]+|(?:%s))\\s(?:[0-9]+|(?:%s))", month_fmt, month_fmt, month_fmt)
  out$date_cleaned <- gsub(sep_regex, "\\1-\\3-\\5", out$date_cleaned, ignore.case = T) #fix sep character for full dates

  ## handle null values
  out$date_cleaned <- gsub("^([0-9][0-9][0-9][0-9])-(00|0)-([0-9][0-9]|[0-9])(\\s|/|T|$)", "\\1\\4", out$date_cleaned)#remove 00 months (Ymd)
  out$date_cleaned <- gsub("^([0-9][0-9]|[0-9])-(00|0)-([0-9][0-9][0-9][0-9])", "\\3", out$date_cleaned)#remove 00 month (dmY)
  out$date_cleaned <- gsub("^([0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9]))-(00|0)(\\s|/|T|$)", "\\1", out$date_cleaned)#remove 00 day (Ymd)
  out$date_cleaned <- gsub("^(00|0)-([0-9][0-9]|[0-9])-([0-9][0-9][0-9][0-9])", "\\2-\\3", out$date_cleaned)#remove 00 day (dmY)

  #handle double dates
  out$date_cleaned <- gsub("^([0-9]{4}(?:-[0-9]{1,2}(?:-[0-9]{1,2}(?:[T\\s][0-9]{2}:[0-9]{2}:[0-9]{2})?)?)?)\\s\\1", "\\1", out$date_cleaned) # collapse double dates if they are equal


  # parse dates
  ## parse basic numeric dates
  out$parsed_format <- NA

  out$parsed_format <- ifelse(grepl(datetime_formats["Y"], out$date_cleaned), "Y", out$parsed_format)
  out$parsed_format <- ifelse(grepl(datetime_formats["Ym"], out$date_cleaned), "Ym", out$parsed_format)
  out$parsed_format <- ifelse(grepl(datetime_formats["mY"], out$date_cleaned), "mY", out$parsed_format)
  out$parsed_format <- ifelse(grepl(datetime_formats["Ymd"], out$date_cleaned), "Ymd", out$parsed_format)
  out$parsed_format <- ifelse(grepl(datetime_formats["dmY"], out$date_cleaned), "dmY", out$parsed_format)

  ## parse dates with month text
  out$parsed_format <- ifelse(grepl(datetime_formats["dmonthY"], out$date_cleaned, ignore.case = T), "dmonthY", out$parsed_format) # for month words
  out$parsed_format <- ifelse(grepl(datetime_formats["monthdY"], out$date_cleaned, ignore.case = T), "monthdY", out$parsed_format) # for month words
  out$parsed_format <- ifelse(grepl(datetime_formats["Ymonthd"], out$date_cleaned, ignore.case = T), "Ymonthd", out$parsed_format) # for month words
  out$parsed_format <- ifelse(grepl(datetime_formats["Ydmonth"], out$date_cleaned, ignore.case = T), "Ydmonth", out$parsed_format) # for month words


  ## parse datetimes
  out[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]", out$date_cleaned),"parsed_format"] <- "YmdT"
  out[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9](\\+|-)", out$date_cleaned),"parsed_format"] <- "YmdTz"

  ## parse errors
  out$parsing_error <- NA
  out[is.na(out$date_raw),"parsing_error"] <- "blank_date" # flag blank dates
  if ("" %in% out$date_raw){
    out[out$date_raw=="","parsing_error"] <- "blank_date" # flag blank dates
  }
  out[grep("^(-|(0|00|0000)(/|\\.|-)(0|00)(/|\\.|-)(0|00|0000))$", out$date_cleaned), "parsing_error"] <- "null_date" # flag null dates
  out[grep("^[0-9][0-9][0-9][0-9]-(1[3-9]|[2-9][0-9])($|-)", out$date_cleaned),"parsing_error"] <- "bad_month" #for Ymd or Ym
  out[grep("^([0-9][0-9]|[0-9])-(1[3-9]|[2-9][0-9])-[0-9][0-9][0-9][0-9]", out$date_cleaned),"parsing_error"] <- "bad_month" #for dmY
  out[grep("^([0-9]{4}(?:-[0-9]{1,2}(?:-[0-9]{1,2}(?:[T\\s][0-9]{2}:[0-9]{2}:[0-9]{2})?)?)?)\\s(?!\\1)", out$date_cleaned, perl = T), "parsing_error"] <- "two_dates" #TODO when double dates aren't equal; right now this will flag if any text follows the first date + \\s, regardless of actual double date - likely that the double date will be the most common sceanrio though (ex: 1998-01-10/19 1998-01-10 19 "two_dates")
  out[is.na(out$parsed_format)&is.na(out$parsing_error),"parsing_error"] <- "unknown_error"







  # out <- fuzzyjoin::regex_left_join(out, data.frame(fmt=names(datetime_formats), regex=datetime_formats), by=c(date_raw="regex"), ignore_case = T)

  #
  #
  # out$parsed_format <- ifelse(grepl("^[0-9][0-9][0-9][0-9]$", out$date_cleaned), "Y", out$parsed_format)
  # out$parsed_format <- ifelse(grepl("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])$", out$date_cleaned), "Ym", out$parsed_format)
  # out$parsed_format <- ifelse(grepl("^([0-9][0-9]|[0-9])-[0-9][0-9][0-9][0-9]$", out$date_cleaned), "mY", out$parsed_format)
  # out$parsed_format <- ifelse(grepl("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])$", out$date_cleaned), "Ymd", out$parsed_format)
  # out$parsed_format <- ifelse(grepl("^([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])-[0-9][0-9][0-9][0-9]$", out$date_cleaned), "dmY", out$parsed_format)
  #
  # ## parse dates with month text
  # out$parsed_format <- ifelse(grepl(sprintf("^([0-9][0-9]|[0-9])-%s-[0-9][0-9][0-9][0-9]$", months_str), out$date_cleaned), "dmonthY", out$parsed_format) # for month words
  # out$parsed_format <- ifelse(grepl(sprintf("^%s-([0-9][0-9]|[0-9])-[0-9][0-9][0-9][0-9]$", months_str), out$date_cleaned), "monthdY", out$parsed_format) # for month words
  # out$parsed_format <- ifelse(grepl(sprintf("^[0-9][0-9][0-9][0-9]-%s-([0-9][0-9]|[0-9])$", months_str), out$date_cleaned), "Ymonthd", out$parsed_format) # for month words
  # out$parsed_format <- ifelse(grepl(sprintf("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-%s$", months_str), out$date_cleaned), "Ydmonth", out$parsed_format) # for month words
  #
  #
  # ## parse datetimes
  # out[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]", out$date_cleaned),"parsed_format"] <- "YmdT"
  # out[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9](\\+|-)", out$date_cleaned),"parsed_format"] <- "YmdTz"
  #
  # ## parse errors
  # out$parsing_error <- NA
  # out[is.na(out$date_raw),"parsing_error"] <- "blank_date" # flag blank dates
  # if ("" %in% out$date_raw){
  #   out[out$date_raw=="","parsing_error"] <- "blank_date" # flag blank dates
  # }
  # out[grep("^(-|(0|00|0000)(/|\\.|-)(0|00)(/|\\.|-)(0|00|0000))$", out$date_cleaned), "parsing_error"] <- "null_date" # flag null dates
  # out[grep("^[0-9][0-9][0-9][0-9]-(1[3-9]|[2-9][0-9])($|-)", out$date_cleaned),"parsing_error"] <- "bad_month" #for Ymd or Ym
  # out[grep("^([0-9][0-9]|[0-9])-(1[3-9]|[2-9][0-9])-[0-9][0-9][0-9][0-9]", out$date_cleaned),"parsing_error"] <- "bad_month" #for dmY
  # out[grep("^([0-9]{4}(?:-[0-9]{1,2}(?:-[0-9]{1,2}(?:[T\\s][0-9]{2}:[0-9]{2}:[0-9]{2})?)?)?)\\s(?!\\1)", out$date_cleaned, perl = T), "parsing_error"] <- "two_dates" #TODO when double dates aren't equal; right now this will flag if any text follows the first date + \\s, regardless of actual double date - likely that the double date will be the most common sceanrio though (ex: 1998-01-10/19 1998-01-10 19 "two_dates")
  # out[is.na(out$parsed_format)&is.na(out$parsing_error),"parsing_error"] <- "unknown_error"








  # TODO don't subset, use different method; if subset return empty df, this doesn't work
  # out[grep("^[0-9][0-9][0-9][0-9]$", out$date_cleaned),"parsed_format"] <- "Y"
  # out[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])$", out$date_cleaned),"parsed_format"] <- "Ym"
  # out[grep("^([0-9][0-9]|[0-9])-[0-9][0-9][0-9][0-9]$", out$date_cleaned),"parsed_format"] <- "mY"
  # out[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])$", out$date_cleaned),"parsed_format"] <- "Ymd"
  # out[grep("^([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])-[0-9][0-9][0-9][0-9]$", out$date_cleaned),"parsed_format"] <- "dmY"


  # out[grep(sprintf("^([0-9][0-9]|[0-9])-%s-[0-9][0-9][0-9][0-9]$", months_str), out$date_cleaned),"parsed_format"] <- "dmonthY" # for month words
  # out[grep(sprintf("^%s-([0-9][0-9]|[0-9])-[0-9][0-9][0-9][0-9]$", months_str), out$date_cleaned),"parsed_format"] <- "monthdY" # for month words
  # out[grep(sprintf("^[0-9][0-9][0-9][0-9]-%s-([0-9][0-9]|[0-9])$", months_str), out$date_cleaned),"parsed_format"] <- "Ymonthd" # for month words
  # out[grep(sprintf("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-%s$", months_str), out$date_cleaned),"parsed_format"] <- "Ydmonth" # for month words





  # out[grep("^([0-9]{4}-[0-9]{1,2})\\s(?!\\1)", out$date_cleaned, perl = T), "parsing_error"] <- "two_months" #when double dates aren't equal
  # out[grep("^([0-9]{4})\\s(?!\\1)", out$date_cleaned, perl = T), "parsing_error"] <- "two_years" #when double dates aren't equal

  # out[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])((T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]|)\\s[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])((T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]|)", out$date_cleaned), "parsing_error"] <- "two_dates" #when double dates aren't equal

 # out$date_parsed <- lubridate::parse_date_time(out$date_cleaned, c("Ymd", "dmY", "Y", "Ym", "mY", "YmdTz", "YmdT"), truncated = 5)
  # out$year_parsed <- lubridate::parse_date_time(out$date_cleaned, c("Y"))
  # out$month_parsed <- lubridate::parse_date_time(out$date_cleaned, c("Ym", "mY"))



  # out$date_harmonized <- ifelse(grepl("^(-|(0|00|0000)(/|\\.|-)(0|00)(/|\\.|-)(0|00|0000))$", out$date_raw), NA, "")

  # out$date_parsed <- lubridate::parse_date_time(out$date_cleaned, c("Ymd", "dmY", "Y", "Ym", "mY", "YmdTz", "YmdT"))


  # out$parsed_format <- ifelse(is.na(out$date_harmonized), NA, "")


  # clean dates
  # out$date_harmonized <- gsub("^([0-9]+)(\\.|/)([0-9]+)(\\.|/)([0-9].*)", "\\1-\\3-\\5", out$date_harmonized)#fix sep character

  #
  # out$date_harmonized <- gsub("^([0-9][0-9][0-9][0-9])-(00|0)-([0-9][0-9]|[0-9])(\\s|/|T|$)", "\\1\\4", out$date_harmonized)#remove 00 months (Ymd)
  # out$date_harmonized <- gsub("^([0-9][0-9]|[0-9])-(00|0)-([0-9][0-9][0-9][0-9])", "\\3", out$date_harmonized)#remove 00 month (dmY)
  # out$date_harmonized <- gsub("^([0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9]))-(00|0)(\\s|/|T|$)", "\\1", out$date_harmonized)#remove 00 day (Ymd)
  # out$date_harmonized <- gsub("^(00|0)-([0-9][0-9]|[0-9])-([0-9][0-9][0-9][0-9])", "\\2-\\3", out$date_harmonized)#remove 00 day (dmY)
  # out$date_harmonized <- gsub("^([0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(|(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]).*)/\\1", "\\1", out$date_harmonized)#fix double dates
  # out$date_harmonized <- gsub("(T|\\s)00:00:00(|\\.0+)", "", out$date_harmonized)#fix null time
  #
  #
  # out[grep("^[0-9][0-9][0-9][0-9]$", out$date_harmonized),"parsed_format"] <- "Y"
  # out[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])$", out$date_harmonized),"parsed_format"] <- "Ym"
  # out[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])$", out$date_harmonized),"parsed_format"] <- "Ymd"
  # out[grep("^[0-9][0-9][0-9][0-9]-(1[3-9]|[2-9][0-9])($|-)", out$date_harmonized),"parsed_format"] <- "bad_month" #for Ymd or Ym
  # out[grep("^([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])-[0-9][0-9][0-9][0-9]$", out$date_harmonized),"parsed_format"] <- "dmY"
  # out[grep(tolower(paste(paste0("([0-9][0-9]|[0-9])\\s", months$word), paste0("([0-9][0-9]|[0-9])\\s", months$abbr), collapse="|", sep="|")), tolower(out$date_harmonized)),"parsed_format"] <- "dmY" #dmonthY - for month words
  # out[grep("^([0-9][0-9]|[0-9])-(1[3-9]|[2-9][0-9])-[0-9][0-9][0-9][0-9]", out$date_harmonized),"parsed_format"] <- "bad_month" #for dmY
  # out[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]", out$date_harmonized),"parsed_format"] <- "YmdT"
  # out[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9](\\+|-)", out$date_harmonized),"parsed_format"] <- "YmdTz"
  # out[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])((T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]|)/[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])((T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]|)", out$date_harmonized),"parsed_format"] <- "double_date" #when double dates aren't equal
  #
  # rows1 <- nrow(out)
  # out <- out[out$parsed_format!="null",]
  # null_rows <- rows1-nrow(out)
  # out <- out[!out$parsed_format%in%c("", "bad_month", "double_date"),]
  # error1_rows <- rows1-null_rows-nrow(out)
  # out <- out[grep(date_res, out$parsed_format),]
  # res_rows <- rows1-null_rows-error1_rows-nrow(out)
  # unique_formats <- unique(out$parsed_format)
  # print(unique_formats)
  # out$date_parsed <- lubridate::parse_date_time(out$date_harmonized, unique_formats[unique_formats!="null"])
  # # out <- out[!is.na(out$date_parsed),]
  # # error2_rows <- rows1-null_rows-error1_rows-res_rows-nrow(out)
  # out$year_parsed <- lubridate::year(out$date_parsed)
  # out$month_parsed <- lubridate::month(out$date_parsed)
  # out$day_parsed <- lubridate::day(out$date_parsed)
  #print(paste0("null_rows=", null_rows,"; error1_rows=", error1_rows,"; res_rows=",res_rows, "; error2_rows=", error2_rows, "; total_removed=", rows1-nrow(out)))
  # message(paste0("Total records removed: ", rows1-nrow(out)))
  return(out)
}
