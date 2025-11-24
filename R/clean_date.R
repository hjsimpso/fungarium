#' @title Clean and harmonize heterogeneous dates
#'
#' @description
#' Parses dates with varying formats into a consistent format and extracts year,
#' month, and day.
#'
#' @param data `Data.frame`.
#' @param date_col Character. Name of column that contains dates.
#'
#' @return Input `data.frame` with the following fields appended.
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
#' data(agaricales) #import sample data set
#' clean_dates <- clean_date(agaricales) #clean dates
#'

clean_date <- function(data,
                       date_col="eventDate"){ # TODO make parsing_error something more date-specific (e.g., date_error)
  # check args
  checkmate::assert_data_frame(data)
  checkmate::assert_character(date_col, max.len = 1)
  checkmate::assert_choice(date_col, colnames(data))
  
  # Call C++ function internally
  data <- cbind(data, clean_dates_cpp(data[[date_col]]))

  # return parsed data
  return(data)
}

