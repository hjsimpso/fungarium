//======================================================================
// Imports
//======================================================================

#include <Rcpp.h>
#include <regex>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <cctype>
#include <ctime>
#include <chrono>
#include <optional> // For std::optional

using namespace Rcpp;

//======================================================================
// Clean date helpers
//======================================================================

std::map<std::string, int> month_name_map = {
    {"jan", 1}, {"january", 1}, {"feb", 2}, {"february", 2},
    {"mar", 3}, {"march", 3}, {"apr", 4}, {"april", 4},
    {"may", 5}, {"jun", 6}, {"june", 6}, {"jul", 7}, {"july", 7},
    {"aug", 8}, {"august", 8}, {"sep", 9}, {"september", 9},
    {"oct", 10}, {"october", 10}, {"nov", 11}, {"november", 11},
    {"dec", 12}, {"december", 12}
};

int get_month_from_name(const std::string& month_raw) {
    std::string m = month_raw;
    transform(m.begin(), m.end(), m.begin(), ::tolower);
    if (month_name_map.find(m) != month_name_map.end()) {
        return month_name_map[m];
    }
    return -1;
};

bool isTwoDatesIdentical(const std::regex& pattern, std::string& cleaned_date, std::string& error) {
    std::smatch match;
    if (regex_match(cleaned_date, match, pattern)) {
        std::string d1 = match[1];
        std::string d2 = match[6];
        if (d1 == d2) {
            cleaned_date = d1;
            return true;
        } else {
            error = "two_dates";
            return false;
        }
    }
    return false;
}


//======================================================================
// Date Cleaning Result Container
//======================================================================
struct DateResult {
    std::string date_raw;
    std::string date_cleaned;
    std::string detected_format;
    std::string parsing_error;
    std::optional<int> year_parsed;
    std::optional<int> month_parsed;
    std::optional<int> day_parsed;
    std::string date_parsed;

    // create date string (YYYY-MM-DD) from year, month, day integers
    std::string format_date() {
        // Format date as YYYY-MM-DD
        if (year_parsed.has_value()&&month_parsed.has_value()&&day_parsed.has_value()){
            std::string year_prefix;
            if (year_parsed<1000){
                year_prefix = "0";
            }else if (year_parsed<100)
            {
                year_prefix = "00";
            }else if (year_parsed<10)
            {
                year_prefix = "000";
            }
            
            date_parsed = year_prefix + std::to_string(year_parsed.value()) + "-";
            date_parsed += (month_parsed.value() < 10 ? "0" : "") + std::to_string(month_parsed.value()) + "-";
            date_parsed += (day_parsed.value() < 10 ? "0" : "") + std::to_string(day_parsed.value());
        }
    }
};


//======================================================================
// Regex strings
//======================================================================

std::string Y_fmt = "[0-9]{4}";
std::string m_fmt = "(?:0?[1-9]|1[0-2])";
std::string d_fmt = "(?:0?[1-9]|1[0-9]|2[0-9]|3[0-1])";
std::string T_fmt = "[T\\s][0-9]{2}:[0-9]{2}:[0-9]{2}";
std::string z_fmt = "(?:Z|[\\+-][0-9]{2}:?[0-9]{2})";
std::string month_fmt = "Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?";

struct DatetimeRegex {
    std::regex Y = std::regex("^" + Y_fmt + "$");
    std::regex Ym = std::regex("^" + Y_fmt + "-" + m_fmt + "$");
    std::regex mY = std::regex("^" + m_fmt + "-" + Y_fmt + "$");
    std::regex Ymd = std::regex("^" + Y_fmt + "-" + m_fmt + "-" + d_fmt + "$");
    std::regex dmY = std::regex("^" + d_fmt + "-" + m_fmt + "-" + Y_fmt + "$");
    std::regex dmonthY = std::regex("^" + d_fmt + "-" + month_fmt + "-" + Y_fmt + "$");
    std::regex monthdY = std::regex("^" + month_fmt + "-" + d_fmt + "-" + Y_fmt + "$");
    std::regex Ymonthd = std::regex("^" + Y_fmt + "-" + month_fmt + "-" + d_fmt + "$");
    std::regex Ydmonth = std::regex("^" + Y_fmt + "-" + d_fmt + "-" + month_fmt + "$");
    std::regex YmdT = std::regex("^" + Y_fmt + "-" + m_fmt + "-" + d_fmt + T_fmt + "$");
    std::regex YmdTz = std::regex("^" + Y_fmt + "-" + m_fmt + "-" + d_fmt + T_fmt + z_fmt + "$");
};

struct ErrorRegex  {
    std::regex null_date = std::regex("^(?:-|(?:0(?:0(?:00)?)?)-(?:0(?:0)?)-(?:0(?:0(?:00)?)?))$");
    std::regex bad_month = std::regex("^(?:[0-9]{4}-(?:1[3-9]|[2-9][0-9])(?:$|-))|(?:^[0-9]{1,2}-(?:1[3-9]|[2-9][0-9])-[0-9]{4})");
};

std::regex two_dates_regex = std::regex("^(" + Y_fmt + "(?:-" + m_fmt + "(?:-" + d_fmt + "(?:" + T_fmt + "(?:" + z_fmt + ")?)?)?)?)\\s(" + Y_fmt + "(?:-" + m_fmt + "(?:-" + d_fmt + "(?:" + T_fmt + "(?:" + z_fmt + ")?)?)?)?)$");


// [[Rcpp::export]]

//======================================================================
// Clean dates function
//======================================================================
DataFrame clean_dates_cpp(const std::vector<std::string>& input_dates) {

    // create containers for output data columns
    std::vector<std::string> date_cleaned_v;
    std::vector<std::string> detected_format_v;
    std::vector<std::string> parsing_error_v;
    std::vector<std::optional<int>> year_parsed_v;
    std::vector<std::optional<int>> month_parsed_v;
    std::vector<std::optional<int>> day_parsed_v;
    std::vector<std::string> date_parsed_v;

    DatetimeRegex datetime_regex;
    ErrorRegex error_regex;

    // iterate through each raw date
    for (const std::string& raw_date : input_dates) {

        // create parsing results container for this date
        DateResult res;
        res.date_raw = raw_date;

        // clean up date strings
        res.date_cleaned = std::regex_replace(raw_date, std::regex("[./\\\\]"), " "); // sub special chars with spaces
        res.date_cleaned = std::regex_replace(res.date_cleaned, std::regex(" {2,}"), " "); // sub 2+ spaces with one space
        res.date_cleaned = std::regex_replace(res.date_cleaned, std::regex("^\\s+|\\s+$"), ""); // remove leading or trailing white space


        // harmonize separator: replace space-separated date parts with dashes
        std::regex sep_regex("^([0-9]+|(?:" + month_fmt + "))\\s([0-9]+|(?:" + month_fmt + "))\\s([0-9]+|(?:" + month_fmt + "))", std::regex_constants::icase);
        res.date_cleaned = std::regex_replace(res.date_cleaned, sep_regex, "$1-$2-$3");

        // handle null values
        // Remove 00 month or 00-00 month-day (Ymd) - save Y
        res.date_cleaned = std::regex_replace(res.date_cleaned, std::regex("^(" + Y_fmt + ")-(?:0{1,2})-(?:0{1,2}|" + d_fmt + ")(?:" + T_fmt + "(?:" + z_fmt + ")?)?$"), "$1");
        // Remove 00 month or 00-00 day-month (dmY) - save Y
        res.date_cleaned = std::regex_replace(res.date_cleaned, std::regex("^(?:0{1,2}|" + d_fmt + ")-(?:0{1,2})-(" + Y_fmt + ")(?:" + T_fmt + "(?:" + z_fmt + ")?)?$"), "$1");
        // Remove 00 day (Ymd) - save Ym
        res.date_cleaned = std::regex_replace(res.date_cleaned, std::regex("^(" + Y_fmt + "-" + m_fmt + ")-(?:0{1,2})(?:" + T_fmt + "(?:" + z_fmt + ")?)?$"), "$1");
        // Remove 00 day (dmY) - save mY
        res.date_cleaned = std::regex_replace(res.date_cleaned, std::regex("^(?:0{1,2})-(" + m_fmt + "-" + Y_fmt + ")(?:" + T_fmt + "(?:" + z_fmt + ")?)?$"), "$1");
        // Remove null Y or Ym or mY
        if (std::regex_match(res.date_cleaned, std::regex("^00(?:00)?-00(?:00)?$"))) {
            res.date_cleaned = "";
        }

        // handle double dates
        bool is_duplicate = isTwoDatesIdentical(two_dates_regex, res.date_cleaned, res.parsing_error);

        // regex serach for predefined date formats
        if (res.date_cleaned.empty()) { // no date
            res.parsing_error = "null_date";
        } else if (std::regex_match(res.date_cleaned, datetime_regex.Y)) { // Y
            res.detected_format = "Y";
            res.year_parsed = std::stoi(res.date_cleaned);
        } else if (std::regex_match(res.date_cleaned, datetime_regex.Ym)) { // Y-m
            res.detected_format = "Ym";
            res.year_parsed = std::stoi(res.date_cleaned.substr(0, 4));
            res.month_parsed = std::stoi(res.date_cleaned.substr(5, 2));
        } else if (std::regex_match(res.date_cleaned, datetime_regex.mY)) { // m-Y
            res.detected_format = "mY";
            res.year_parsed = std::stoi(res.date_cleaned.substr(3, 4));
            res.month_parsed = std::stoi(res.date_cleaned.substr(0, 2));
        } else if (std::regex_match(res.date_cleaned, datetime_regex.Ymd)) { // Y-m-d (ISO 8601 standard)
            res.detected_format = "Ymd";
            res.year_parsed = std::stoi(res.date_cleaned.substr(0, 4));
            res.month_parsed = std::stoi(res.date_cleaned.substr(5, 2));
            res.day_parsed = std::stoi(res.date_cleaned.substr(8, 2));
        } else if (std::regex_match(res.date_cleaned, datetime_regex.dmY)) { // d-m-Y
            res.detected_format = "dmY";
            res.day_parsed = std::stoi(res.date_cleaned.substr(0, 2));
            res.month_parsed = std::stoi(res.date_cleaned.substr(3, 2));
            res.year_parsed = std::stoi(res.date_cleaned.substr(6, 4));
        } else if (std::regex_match(res.date_cleaned, datetime_regex.dmonthY)) { // d-month-Y
          // dmonthY: 13-Jan-2024
          std::vector<std::string> parts;
          std::stringstream ss(res.date_cleaned);
          std::string segment;
          while (getline(ss, segment, '-')) parts.push_back(segment);
          int d = stoi(parts[0]);
          int m = get_month_from_name(parts[1]);
          int y = stoi(parts[2]);
          if (m > 0) {
            res.detected_format = "dmonthY";
            res.year_parsed = y;
            res.month_parsed = m;
            res.day_parsed = d;
          }
        }
        else if (std::regex_match(res.date_cleaned, datetime_regex.monthdY)) { // month-d-Y
            // monthdY: Jan 13 2024
            std::vector<std::string> parts;
            std::stringstream ss(res.date_cleaned);
            std::string segment;
            while (getline(ss, segment, '-')) parts.push_back(segment);
            int m = get_month_from_name(parts[0]);
            int d = stoi(parts[1]);
            int y = stoi(parts[2]);
            if (m > 0) {
                res.detected_format = "monthdY";
                res.year_parsed = y;
                res.month_parsed = m;
                res.day_parsed = d;
            }
        }
        else if (std::regex_match(res.date_cleaned, datetime_regex.Ymonthd)) { // Y-month-d
            // Ymonthd: 2024 January 13
            std::vector<std::string> parts;
            std::stringstream ss(res.date_cleaned);
            std::string segment;
            while (getline(ss, segment, '-')) parts.push_back(segment);
            int y = stoi(parts[0]);
            int m = get_month_from_name(parts[1]);
            int d = stoi(parts[2]);
            if (m > 0) {
                res.detected_format = "Ymonthd";
                res.year_parsed = y;
                res.month_parsed = m;
                res.day_parsed = d;
            }
        }
        else if (std::regex_match(res.date_cleaned, datetime_regex.YmdT)) { // Y-m-d T
            // ISO w/o time zone
            res.detected_format = "YmdT";
            res.year_parsed = std::stoi(res.date_cleaned.substr(0, 4));
            res.month_parsed = std::stoi(res.date_cleaned.substr(5, 2));
            res.day_parsed = std::stoi(res.date_cleaned.substr(8, 2));
            res.date_parsed = std::stoi(res.date_cleaned.substr(0, 10)); // Y-m-d
        }
        else if (std::regex_match(res.date_cleaned, datetime_regex.YmdTz)) { // Y-m-d T z
            // ISO with time zone
            res.detected_format = "YmdTz";
            res.year_parsed = std::stoi(res.date_cleaned.substr(0, 4));
            res.month_parsed = std::stoi(res.date_cleaned.substr(5, 2));
            res.day_parsed = std::stoi(res.date_cleaned.substr(8, 2));
            res.date_parsed = std::stoi(res.date_cleaned.substr(0, 10)); // Y-m-d
        }
        else {
            res.parsing_error = "undefined_error";
        }

        // create date string from year, month, day ints
        res.format_date();
        

    // Append date results to results vectors
        date_cleaned_v.push_back(res.date_raw);  
        detected_format_v.push_back(res.detected_format);
        parsing_error_v.push_back(res.parsing_error);
        year_parsed_v.push_back(res.year_parsed);    
        month_parsed_v.push_back(res.month_parsed);
        day_parsed_v.push_back(res.day_parsed);
        date_parsed_v.push_back(res.date_parsed);
    }

    // create output dataframe containg parsed date vectors
    return DataFrame::create(
        Named("date_raw") = input_dates,
        // Named("cleaned") = date_cleaned_v, // dont output cleaned dates?
        Named("detected_format") = detected_format_v,
        Named("parsing_error") = parsing_error_v,
        Named("year_parsed") = year_parsed_v,
        Named("month_parsed") = month_parsed_v,
        Named("day_parsed") = day_parsed_v,
        Named("date_parsed") = date_parsed_v,
        Named("stringsAsFactors") = false
    );
};



