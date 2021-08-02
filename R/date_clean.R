
##date clean
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
  data$date_flag <- ifelse(data$date_fixed=="", "null", "")
  data$date_fixed <- gsub("^([0-9]+)(\\.|/)([0-9]+)(\\.|/)([0-9].*)", "\\1-\\3-\\5", data$date_fixed)#fix sep character
  data$date_fixed <- gsub("^([0-9][0-9][0-9][0-9])-(00|0)-([0-9][0-9]|[0-9])(\\s|/|T|$)", "\\1\\4", data$date_fixed)#remove 00 months (Ymd)
  data$date_fixed <- gsub("^([0-9][0-9]|[0-9])-(00|0)-([0-9][0-9][0-9][0-9])", "\\3", data$date_fixed)#remove 00 month (dmY)
  data$date_fixed <- gsub("^([0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9]))-(00|0)(\\s|/|T|$)", "\\1", data$date_fixed)#remove 00 day
  data$date_fixed <- gsub("^(00|0)-([0-9][0-9]|[0-9])-([0-9][0-9][0-9][0-9])", "\\2-\\3", data$date_fixed)#remove 00 day (dmY)
  data$date_fixed <- gsub("^([0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(|(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]).*)/\\1", "\\1", data$date_fixed)#fix double dates
  data$date_fixed <- gsub("(T|\\s)00:00:00(|\\.0+)", "", data$date_fixed)#fix null time

  data[grep("^[0-9][0-9][0-9][0-9]$", data$date_fixed),"date_flag"] <- "Y"
  data[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])$", data$date_fixed),"date_flag"] <- "Ym"
  data[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])$", data$date_fixed),"date_flag"] <- "Ymd"
  data[grep("^[0-9][0-9][0-9][0-9]-(1[3-9]|[2-9][0-9])($|-)", data$date_fixed),"date_flag"] <- "bad_month" #for Ymd or Ym
  data[grep("^([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])-[0-9][0-9][0-9][0-9]$", data$date_fixed),"date_flag"] <- "dmY"
  data[grep(tolower(paste(paste0("([0-9][0-9]|[0-9])\\s", months$word), paste0("([0-9][0-9]|[0-9])\\s", months$abbr), collapse="|", sep="|")), tolower(data$date_fixed)),"date_flag"] <- "dmY" #dmonthY - for month words
  data[grep("^([0-9][0-9]|[0-9])-(1[3-9]|[2-9][0-9])-[0-9][0-9][0-9][0-9]", data$date_fixed),"date_flag"] <- "bad_month" #for dmY
  data[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]", data$date_fixed),"date_flag"] <- "YmdT"
  data[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])(T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9](\\+|-)", data$date_fixed),"date_flag"] <- "YmdTz"
  data[grep("^[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])((T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]|)/[0-9][0-9][0-9][0-9]-([0-9][0-9]|[0-9])-([0-9][0-9]|[0-9])((T|\\s)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]|)", data$date_fixed),"date_flag"] <- "double_date" #when double dates aren't equal
  rows1 <- nrow(data)
  data <- data[data$date_flag!="null",]
  null_rows <- rows1-nrow(data)
  data <- data[!data$date_flag%in%c("", "bad_month", "double_date"),]
  error1_rows <- rows1-null_rows-nrow(data)
  data <- data[grep(date_res, data$date_flag),]
  res_rows <- rows1-null_rows-error1_rows-nrow(data)
  data$parsed_dates <- lubridate::parse_date_time(data$date_fixed, unique(data$date_flag))
  data <- data[!is.na(data$parsed_dates),]
  error2_rows <- rows1-null_rows-error1_rows-res_rows-nrow(data)
  if(date_res=="Y"){
    data$year_fixed <- lubridate::year(data$parsed_dates)
  }else if(date_res=="m"){
    data$year_fixed <- lubridate::year(data$parsed_dates)
    data$month_fixed <- lubridate::month(data$parsed_date)
  }else if(date_res=="d"){
    data$year_fixed <- lubridate::year(data$parsed_dates)
    data$month_fixed <- lubridate::month(data$parsed_date)
    data$day_fixed <- lubridate::day(data$parsed_date)
  }
  print(paste0("null_rows=", null_rows,"; error1_rows=", error1_rows,"; res_rows=",res_rows, "; error2_rows=", error2_rows, "; total_removed=", rows1-nrow(data)))
  return(data)
}
