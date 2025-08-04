#' Clean heterogeneous dates in occurrence data sets
#'
#' Parses dates with varying formats into a consistent format and extracts year,
#' month, and day.
#'
#' @param dates Character. Dates with varying formats (e.g., "1990-01-24",
#' "13-Jan-2024", "03/12/2003").
#'
#' @return Data.frame with the following output fields.
#'
#' \describe{
#' \item{\code{date_raw}}{Character. Input date.}
#' \item{\code{detected_format}}{Character. Detected date format (e.g., "Y", "Ym", or "Ymd").}
#' \item{\code{parsing_error}}{Character. Error that prevented successful parsing.}
#' \item{\code{year_parsed}}{Integer. Year parsed into harmonized format (YYYY).}
#' \item{\code{month_parsed}}{Integer. Month parsed into harmonized format (MM).}
#' \item{\code{day_parsed}}{Integer. Day parsed into harmonized format (DD).}
#' \item{\code{date_parsed}}{Date. Dates parsed into harmonized format (YYYY-MM-DD).}
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
#' clean_dates <- clean_dates(agaricales_updated$eventDate) #clean dates
#'


clean_dates <- function(dates){
  # check args
  checkmate::assert_character(dates)
  checkmate::assert_true(length(dates)>0)

  # reference data
  Y_fmt <- "[0-9]{4}"
  m_fmt <- "(?:0?[1-9]|1[0-2])"
  d_fmt <- "(?:0?[1-9]|1[0-9]|2[0-9]|3[0-1])"
  T_fmt <- "[T\\s][0-9]{2}:[0-9]{2}:[0-9]{2}"
  z_fmt <- "(?:Z|[\\+-][0-9]{2}:?[0-9]{2})"
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
                        "YmdT" = sprintf("^%s-%s-%s%s$", Y_fmt, m_fmt, d_fmt, T_fmt),
                        "YmdTz" = sprintf("^%s-%s-%s%s%s$", Y_fmt, m_fmt, d_fmt, T_fmt, z_fmt)
  )

  error_formats <- c("null_date" = "^(?:-|(?:0(?:0(?:00)?)?)-(?:0(?:0)?)-(?:0(?:0(?:00)?)?))$",
                     "bad_month" = "^(?:[0-9]{4}-(?:1[3-9]|[2-9][0-9])(?:$|-))|(?:^[0-9]{1,2}-(?:1[3-9]|[2-9][0-9])-[0-9]{4})" # assuming Ym(d) or dmY formats
                     )
  two_dates_fmt <- sprintf("^(%s(?:-%s(?:-%s(?:%s(?:%s)?)?)?)?)\\s(%s(?:-%s(?:-%s(?:%s(?:%s)?)?)?)?)$", Y_fmt, m_fmt, d_fmt, T_fmt, z_fmt, Y_fmt, m_fmt, d_fmt, T_fmt, z_fmt)

  # create result data.frame
  out <- data.frame(date_raw = dates)
  out$date_cleaned <- as.character(NA)
  out$detected_format <- as.character(NA)
  out$parsing_error <- as.character(NA)
  out$year_parsed <- as.integer(NA)
  out$month_parsed <- as.integer(NA)
  out$day_parsed <- as.integer(NA)
  out$date_parsed <- as.Date(NA)

  # clean dates
  ## remove special chars
  out$date_cleaned <- gsub("\\.|/|\\\\", " ", out$date_raw) # sub special chars with spaces
  out$date_cleaned <- gsub(" {2,}", " ", out$date_cleaned) # sub 2+ spaces with one space

  ## remove leading or trailing white space
  out$date_cleaned <- gsub("^\\s+|\\s+$", "", out$date_cleaned)

  ## harmonize separator
  sep_regex <- sprintf("^([0-9]+|(?:%s))\\s([0-9]+|(?:%s))\\s([0-9]+|(?:%s))", month_fmt, month_fmt, month_fmt)
  out$date_cleaned <- gsub(sep_regex, "\\1-\\2-\\3", out$date_cleaned, ignore.case = T) #fix sep character for full dates

  ## handle null values
  out$date_cleaned <- gsub(sprintf("^(%s)-(?:0{1,2})-(?:0{1,2}|%s)(?:%s(?:%s)?)?$", Y_fmt, d_fmt, T_fmt, z_fmt), "\\1", out$date_cleaned) # remove 00 month or 00-00 month-day (Ymd) - save Y
  out$date_cleaned <- gsub(sprintf("^(?:0{1,2}|%s)-(?:0{1,2})-(%s)(?:%s(?:%s)?)?$", d_fmt, Y_fmt, T_fmt, z_fmt), "\\1", out$date_cleaned) # remove 00 month or 00-00 day-month (dmY) - save Y
  out$date_cleaned <- gsub(sprintf("^(%s-%s)-(?:0{1,2})(?:%s(?:%s)?)?$", Y_fmt, m_fmt, T_fmt, z_fmt), "\\1", out$date_cleaned) # remove 00 day (Ymd) - save Ym
  out$date_cleaned <- gsub(sprintf("^(?:0{1,2})-(%s-%s)(?:%s(?:%s)?)?$", m_fmt, Y_fmt, T_fmt, z_fmt), "\\1", out$date_cleaned) # remove 00 day (dmY) - save mY
  out$date_cleaned <- gsub("^00(?:00)?-00(?:00)?$", NA, out$date_cleaned) # remove null Y or Ym or mY

  ## handle double dates
  double_dates_bool <- grepl(two_dates_fmt, out$date_cleaned)
  if (T%in%double_dates_bool){
    date_check <- is_duplicate_date(out[double_dates_bool,]$date_cleaned, two_dates_fmt)
    out[double_dates_bool,"date_cleaned"] <- date_check$date_cleaned
    out[double_dates_bool,"parsing_error"] <- date_check$parsing_error
  }

  # parse date format
  ## parse basic numeric dates
  out$detected_format <- ifelse(grepl(datetime_formats["Y"], out$date_cleaned), "Y", out$detected_format)
  out$detected_format <- ifelse(grepl(datetime_formats["Ym"], out$date_cleaned), "Ym", out$detected_format)
  out$detected_format <- ifelse(grepl(datetime_formats["mY"], out$date_cleaned), "mY", out$detected_format)
  out$detected_format <- ifelse(grepl(datetime_formats["Ymd"], out$date_cleaned), "Ymd", out$detected_format)
  out$detected_format <- ifelse(grepl(datetime_formats["dmY"], out$date_cleaned), "dmY", out$detected_format)

  ## parse dates with month text
  out$detected_format <- ifelse(grepl(datetime_formats["dmonthY"], out$date_cleaned, ignore.case = T), "dmonthY", out$detected_format)
  out$detected_format <- ifelse(grepl(datetime_formats["monthdY"], out$date_cleaned, ignore.case = T), "monthdY", out$detected_format)
  out$detected_format <- ifelse(grepl(datetime_formats["Ymonthd"], out$date_cleaned, ignore.case = T), "Ymonthd", out$detected_format)
  out$detected_format <- ifelse(grepl(datetime_formats["Ydmonth"], out$date_cleaned, ignore.case = T), "Ydmonth", out$detected_format)

  ## parse datetimes
  out$detected_format <- ifelse(grepl(datetime_formats["YmdT"], out$date_cleaned, ignore.case = T), "YmdT", out$detected_format)
  out$detected_format <- ifelse(grepl(datetime_formats["YmdTz"], out$date_cleaned, ignore.case = T), "YmdTz", out$detected_format)

  ## parse errors
  out$parsing_error <- ifelse(is.na(out$date_cleaned)|out$date_cleaned=="", "null_date", out$parsing_error)
  out$parsing_error <- ifelse(grepl(error_formats["bad_month"], out$date_cleaned), "bad_month", out$parsing_error)
  out$parsing_error <- ifelse(is.na(out$detected_format)&is.na(out$parsing_error), "undefined_error", out$parsing_error)

  # parse dates to year, month, day fields
  datetime_parsed <- lubridate::parse_date_time(out$date_cleaned, orders=c("Y", "Ym", "mY", "Ymd", "dmY", "YmdT", "YmdTz"), quiet=T)

  year_bool <- grepl("Y", out$detected_format)
  month_bool <- grepl("m(?:onth)?", out$detected_format)
  day_bool <- grepl("d", out$detected_format)
  date_bool <- year_bool&month_bool&day_bool

  if (T %in% date_bool){
    out[date_bool,]$date_parsed <-lubridate::date(datetime_parsed[date_bool])
  }
  if (T %in% year_bool){
    out[year_bool,]$year_parsed <-lubridate::year(datetime_parsed[year_bool])
  }
  if (T %in% month_bool){
    out[month_bool,]$month_parsed <-lubridate::month(datetime_parsed[month_bool])
  }
  if (T %in% day_bool){
    out[day_bool,]$day_parsed <-lubridate::day(datetime_parsed[day_bool])
  }


  # return results dataframe
  return(out[,colnames(out)!="date_cleaned"])
}

is_duplicate_date <- function(x, pattern) {
  # Match the pattern
  matches <- regexec(pattern, x)
  extracted <- regmatches(x, matches)

  # create concatonated list to iterate through
  extracted <- Map(c, x, extracted)

  # Compare group 1 and 2 (return NA if not matched)
  date_check <- lapply(extracted, function(g) {
    if (length(g) == 4) { # is a double date (may or may not be identical dates)
      if (g[3] == g[4]){ # two dates which are identical
        c(T, g[3]) # return 'merged' date
      }else{ # two dates but not identical
        c(F, g[1]) # return original date string
      }
    } else { # not a double date
      c(NA, g[1]) # return original date string
    }
  })
  date_check <- as.data.frame(do.call(rbind, date_check))
  colnames(date_check) <- c("duplicate_date", "date_cleaned")
  date_check$duplicate_date <- as.logical(date_check$duplicate_date)
  date_check$parsing_error <- ifelse(!date_check$duplicate_date, "two_dates", NA)

  return(date_check)
}
