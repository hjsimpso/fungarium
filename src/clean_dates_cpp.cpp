//======================================================================
// Imports
//======================================================================

#include <Rcpp.h>
#include <boost/regex.hpp>
#include <string>
#include <vector>
#include <sstream>
#include <optional> // For std::optional
#include <typeinfo> // Required for typeid
// #include <algorithm>
// #include <cctype>
// #include <ctime>
// #include <chrono>


// using namespace Rcpp;

//======================================================================
// Clean date helpers
//======================================================================

int get_month_from_name(const std::string& month_raw) {
    std::map<std::string, int> month_name_map = {
        {"jan", 1}, {"january", 1}, {"feb", 2}, {"february", 2},
        {"mar", 3}, {"march", 3}, {"apr", 4}, {"april", 4},
        {"may", 5}, {"jun", 6}, {"june", 6}, {"jul", 7}, {"july", 7},
        {"aug", 8}, {"august", 8}, {"sep", 9}, {"september", 9},
        {"oct", 10}, {"october", 10}, {"nov", 11}, {"november", 11},
        {"dec", 12}, {"december", 12}
    };

    std::string m = month_raw;
    transform(m.begin(), m.end(), m.begin(), ::tolower);
    if (month_name_map.find(m) != month_name_map.end()) {
        return month_name_map[m];
    }
    return -1;
};

void isTwoDatesIdentical(const boost::regex& pattern, std::string& cleaned_date, std::string& error) {
    boost::smatch match;
    if (boost::regex_match(cleaned_date, match, pattern)) {
        std::string d1 = match[1];
        std::string d2 = match[6];
        if (d1 == d2) {
            cleaned_date = d1;
        } else {
            error = "two_dates";
        }
    }
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
    void format_date() {
        // Format date as YYYY-MM-DD
        if (year_parsed.has_value()&&month_parsed.has_value()&&day_parsed.has_value()){
            std::string year_prefix;
            if (year_parsed<1000) {
                year_prefix = "0";
            }
            if (year_parsed<100) {
                year_prefix = "00";
            }
            if (year_parsed<10) {
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
struct DateTimeFormats{
    std::string Y_fmt = "[0-9]{4}";
    std::string m_fmt = "(?:0?[1-9]|1[0-2])";
    std::string d_fmt = "(?:0?[1-9]|1[0-9]|2[0-9]|3[0-1])";
    std::string T_fmt = "[T\\s][0-9]{2}:[0-9]{2}:[0-9]{2}";
    std::string z_fmt = "(?:Z|[\\+-][0-9]{2}:?[0-9]{2})";
    std::string month_fmt = "Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?";
};

struct DatetimeRegex {
    DateTimeFormats fmts;

    boost::regex Y = boost::regex("^" + fmts.Y_fmt + "$");
    boost::regex Ym = boost::regex("^" + fmts.Y_fmt + "-" + fmts.m_fmt + "$");
    boost::regex mY = boost::regex("^" + fmts.m_fmt + "-" + fmts.Y_fmt + "$");
    boost::regex Ymd = boost::regex("^" + fmts.Y_fmt + "-" + fmts.m_fmt + "-" + fmts.d_fmt + "$");
    boost::regex dmY = boost::regex("^" + fmts.d_fmt + "-" + fmts.m_fmt + "-" + fmts.Y_fmt + "$");
    boost::regex dmonthY = boost::regex("^" + fmts.d_fmt + "-" + fmts.month_fmt + "-" + fmts.Y_fmt + "$", boost::regex_constants::icase);
    boost::regex monthdY = boost::regex("^(?:" + fmts.month_fmt + ")-" + fmts.d_fmt + "-" + fmts.Y_fmt + "$", boost::regex_constants::icase);
    boost::regex Ymonthd = boost::regex("^" + fmts.Y_fmt + "-" + fmts.month_fmt + "-" + fmts.d_fmt + "$", boost::regex_constants::icase);
    boost::regex Ydmonth = boost::regex("^" + fmts.Y_fmt + "-" + fmts.d_fmt + "-" + fmts.month_fmt + "$", boost::regex_constants::icase);
    boost::regex YmdT = boost::regex("^" + fmts.Y_fmt + "-" + fmts.m_fmt + "-" + fmts.d_fmt + fmts.T_fmt  +"$");
    boost::regex YmdTz = boost::regex("^" + fmts.Y_fmt + "-" + fmts.m_fmt + "-" + fmts.d_fmt + fmts.T_fmt + fmts.z_fmt + "$");
    // std::string Ym = "^" + fmts.Y_fmt + "-" + fmts.m_fmt + "$";
    // std::string mY = "^" + fmts.m_fmt + "-" + fmts.Y_fmt + "$";
    // std::string Ymd = "^" + fmts.Y_fmt + "-" + fmts.m_fmt + "-" + fmts.d_fmt + "$";
    // std::string dmY = "^" + fmts.d_fmt + "-" + fmts.m_fmt + "-" + fmts.Y_fmt + "$";
    // std::string dmonthY = "^" + fmts.d_fmt + "-" + fmts.month_fmt + "-" + fmts.Y_fmt + "$";
    // std::string monthdY = "^" + fmts.month_fmt + "-" + fmts.d_fmt + "-" + fmts.Y_fmt + "$";
    // std::string Ymonthd = "^" + fmts.Y_fmt + "-" + fmts.month_fmt + "-" + fmts.d_fmt + "$";
    // std::string Ydmonth = "^" + fmts.Y_fmt + "-" + fmts.d_fmt + "-" + fmts.month_fmt + "$";
    // std::string YmdT = "^" + fmts.Y_fmt + "-" + fmts.m_fmt + "-" + fmts.d_fmt + fmts.T_fmt  +"$";
    // std::string YmdTz = "^" + fmts.Y_fmt + "-" + fmts.m_fmt + "-" + fmts.d_fmt + fmts.T_fmt + fmts.z_fmt + "$";

    boost::regex two_dates_regex = boost::regex("^(" + fmts.Y_fmt + "(?:-" + fmts.m_fmt + "(?:-" + fmts.d_fmt + "(?:" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?)?)?)\\s(" + fmts.Y_fmt + "(?:-" + fmts.m_fmt + "(?:-" + fmts.d_fmt + "(?:" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?)?)?)$");
};

struct ErrorRegex  {
    std::string null_date = "^(?:-|(?:0(?:0(?:00)?)?)-(?:0(?:0)?)-(?:0(?:0(?:00)?)?))$";
    std::string bad_month = "^(?:[0-9]{4}-(?:1[3-9]|[2-9][0-9])(?:$|-))|(?:^[0-9]{1,2}-(?:1[3-9]|[2-9][0-9])-[0-9]{4})";
};

//======================================================================
// Clean dates function
//======================================================================
// [[Rcpp::depends(BH)]]
// [[Rcpp::export]]
Rcpp::DataFrame clean_dates_cpp(const Rcpp::CharacterVector& input_dates) {
    Rcpp::Rcout << typeid(input_dates).name() << std::endl; // Ensure input_dates is of type CharacterVector
    // TODO make input unique then rejoin at end of function
    //TESTING
    // std::string str1 = "string1";
    // Rcpp::Rcout << "Original: " << str1 << std::endl;
    // boost::regex pattern1("(\\d)"); 
    // std::string replacement1 = "($1)";
    // std::string result1 = boost::regex_replace(str1, pattern1, replacement1);
    // Rcpp::Rcout << "Original: " << str1 << std::endl;
    // Rcpp::Rcout << "Reformatted: " << result1 << std::endl;


    // std::string text = "My phone number is 123-456-7890.";
    // // Matches three digits, a dash, three digits, a dash, and four digits
    // std::regex pattern("(\\d{3})-(\\d{3})-(\\d{4})"); 
    // std::string replacement = "($1) $2-$3"; // Reformat as (XXX) YYY-ZZZZ

    // std::string result = std::regex_replace(text, pattern, replacement);

    // Rcpp::Rcout << "Original: " << text << std::endl;
    // Rcpp::Rcout << "Reformatted: " << result << std::endl;

    // return Rcpp::DataFrame::create(
    //     Rcpp::Named("date_raw") = result1);

    try{
        // create containers for output data columns
        std::vector<std::string> date_cleaned_v;
        std::vector<std::string> detected_format_v;
        std::vector<std::string> parsing_error_v;
        std::vector<std::string> year_parsed_v;
        std::vector<std::string> month_parsed_v;
        std::vector<std::string> day_parsed_v;
        std::vector<std::string> date_parsed_v;

        DatetimeRegex datetime_regex;
        ErrorRegex error_regex;
        
        //TEST
        Rcpp::Rcout << boost::regex_match("Jan-06-1900", datetime_regex.monthdY) << std::endl;
        Rcpp::Rcout << boost::regex_match("Jan-06-1900", boost::regex("^Jan-[0-9]+-[0-9]+$")) << std::endl;

        //////////
        // iterate through each raw date
        for (int i = 0; i < input_dates.size(); i++) {
            std::string raw_date = Rcpp::as<std::string>(input_dates[i]);
            Rcpp::Rcout << typeid(raw_date).name() << std::endl; // Ensure raw_date is of type std::string

            // create parsing results container for this date
            DateResult res;
            res.date_raw = raw_date;

            // clean up date strings
            res.date_cleaned = boost::regex_replace(raw_date, boost::regex("[,./\\\\]"), " "); // sub special chars with spaces
            res.date_cleaned = boost::regex_replace(res.date_cleaned, boost::regex(" {2,}"), " "); // sub 2+ spaces with one space
            res.date_cleaned = boost::regex_replace(res.date_cleaned, boost::regex("^\\s+|\\s+$"), ""); // remove leading or trailing white space

            Rcpp::Rcout << res.date_cleaned << std::endl;

            // Harmonize separator: replace space-separated date parts with dashes
            // Handles Y, Y-m, Y-m-d, and also supports month names
            std::string harmonize_fmt3 = "^([0-9]+|(?:" + datetime_regex.fmts.month_fmt + "))\\s([0-9]+|(?:" + datetime_regex.fmts.month_fmt + "))\\s([0-9]+|(?:" + datetime_regex.fmts.month_fmt + "))$";
            std::string harmonize_fmt2 = "^([0-9]+|(?:" + datetime_regex.fmts.month_fmt + "))\\s([0-9]+|(?:" + datetime_regex.fmts.month_fmt + "))$";
            std::string harmonize_fmt1 = "^([0-9]+|(?:" + datetime_regex.fmts.month_fmt + "))$";

            // Try 3-part (Y m d)
            boost::regex sep_regex3(harmonize_fmt3, boost::regex_constants::icase);
            if (boost::regex_match(res.date_cleaned, sep_regex3)) {
                res.date_cleaned = boost::regex_replace(res.date_cleaned, sep_regex3, "$1-$2-$3");
            } else {
                // Try 2-part (Y m)
                boost::regex sep_regex2(harmonize_fmt2, boost::regex_constants::icase);
                if (boost::regex_match(res.date_cleaned, sep_regex2)) {
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, sep_regex2, "$1-$2");
                } else {
                    // Try 1-part (Y)
                    boost::regex sep_regex1(harmonize_fmt1, boost::regex_constants::icase);
                    if (boost::regex_match(res.date_cleaned, sep_regex1)) {
                        res.date_cleaned = boost::regex_replace(res.date_cleaned, sep_regex1, "$1");
                    }
                }
            }

            Rcpp::Rcout << res.date_cleaned << std::endl;

            // handle null values
            // Remove 00 month or 00-00 month-day (Ymd) - save Y
            std::string null_fmt = "^(" + datetime_regex.fmts.Y_fmt + ")-(?:0{1,2})-(?:0{1,2}|" + datetime_regex.fmts.d_fmt + ")(?:" + datetime_regex.fmts.T_fmt + "(?:" + datetime_regex.fmts.z_fmt + ")?)?$";
            res.date_cleaned = boost::regex_replace(res.date_cleaned, boost::regex(null_fmt), "$1");
            // Remove 00 month or 00-00 day-month (dmY) - save Y
            null_fmt = "^(?:0{1,2}|" + datetime_regex.fmts.d_fmt + ")-(?:0{1,2})-(" + datetime_regex.fmts.Y_fmt + ")(?:" + datetime_regex.fmts.T_fmt + "(?:" + datetime_regex.fmts.z_fmt + ")?)?$";
            res.date_cleaned = boost::regex_replace(res.date_cleaned, boost::regex(null_fmt), "$1");
            // Remove 00 day (Ymd) - save Ym
            null_fmt = "^(" + datetime_regex.fmts.Y_fmt + "-" + datetime_regex.fmts.m_fmt + ")-(?:0{1,2})(?:" + datetime_regex.fmts.T_fmt + "(?:" + datetime_regex.fmts.z_fmt + ")?)?$";
            res.date_cleaned = boost::regex_replace(res.date_cleaned, boost::regex(null_fmt), "$1");
            // Remove 00 day (dmY) - save mY
            null_fmt = "^(?:0{1,2})-(" + datetime_regex.fmts.m_fmt + "-" + datetime_regex.fmts.Y_fmt + ")(?:" + datetime_regex.fmts.T_fmt + "(?:" + datetime_regex.fmts.z_fmt + ")?)?$";
            res.date_cleaned = boost::regex_replace(res.date_cleaned, boost::regex(null_fmt), "$1");
            // Remove null Y or Ym or mY
            null_fmt = std::string("^00(?:00)?(?:-00(?:00)?)?$");
            if (boost::regex_match(res.date_cleaned, boost::regex(null_fmt))) {
                res.date_cleaned = "";
            }
            Rcpp::Rcout << res.date_cleaned << std::endl;

            // handle double dates
            isTwoDatesIdentical(datetime_regex.two_dates_regex, res.date_cleaned, res.parsing_error);

            // regex serach for predefined date formats
            if (res.date_cleaned.empty()) { // no date
                res.parsing_error = "null_date";
            } else if (boost::regex_match(res.date_cleaned, datetime_regex.Y)) { // Y
                res.detected_format = "Y";
                res.year_parsed = std::stoi(res.date_cleaned);
            } else if (boost::regex_match(res.date_cleaned, datetime_regex.Ym)) { // Y-m
                res.detected_format = "Ym";
                res.year_parsed = std::stoi(res.date_cleaned.substr(0, 4));
                res.month_parsed = std::stoi(res.date_cleaned.substr(5, 2));
            } else if (boost::regex_match(res.date_cleaned, datetime_regex.mY)) { // m-Y
                res.detected_format = "mY";
                res.year_parsed = std::stoi(res.date_cleaned.substr(3, 4));
                res.month_parsed = std::stoi(res.date_cleaned.substr(0, 2));
            } else if (boost::regex_match(res.date_cleaned, datetime_regex.Ymd)) { // Y-m-d (ISO 8601 standard)
                res.detected_format = "Ymd";
                res.year_parsed = std::stoi(res.date_cleaned.substr(0, 4));
                res.month_parsed = std::stoi(res.date_cleaned.substr(5, 2));
                res.day_parsed = std::stoi(res.date_cleaned.substr(8, 2));
            } else if (boost::regex_match(res.date_cleaned, datetime_regex.dmY)) { // d-m-Y
                res.detected_format = "dmY";
                res.day_parsed = std::stoi(res.date_cleaned.substr(0, 2));
                res.month_parsed = std::stoi(res.date_cleaned.substr(3, 2));
                res.year_parsed = std::stoi(res.date_cleaned.substr(6, 4));
            } else if (boost::regex_match(res.date_cleaned, datetime_regex.dmonthY)) { // d-month-Y
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
                } else {
                    res.parsing_error = "undefined_error";
                }
            }
            else if (boost::regex_match(res.date_cleaned, datetime_regex.monthdY)) { // month-d-Y
                Rcpp::Rcout << "monthdY" << std::endl;
                // monthdY: Jan 13 2024
                std::vector<std::string> parts;
                std::stringstream ss(res.date_cleaned);
                std::string segment;
                while (getline(ss, segment, '-')) parts.push_back(segment);
                int m = get_month_from_name(parts[0]);
                Rcpp::Rcout << m << std::endl;
                int d = stoi(parts[1]);
                int y = stoi(parts[2]);
                if (m > 0) {
                    res.detected_format = "monthdY";
                    res.year_parsed = y;
                    res.month_parsed = m;
                    res.day_parsed = d;
                } else {
                    res.parsing_error = "undefined_error";
                }
            }
            else if (boost::regex_match(res.date_cleaned, datetime_regex.Ymonthd)) { // Y-month-d
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
                } else {
                    res.parsing_error = "undefined_error";
                }
            }
            else if (boost::regex_match(res.date_cleaned, datetime_regex.YmdT)) { // Y-m-d T
                // ISO w/o time zone
                res.detected_format = "YmdT";
                res.year_parsed = std::stoi(res.date_cleaned.substr(0, 4));
                res.month_parsed = std::stoi(res.date_cleaned.substr(5, 2));
                res.day_parsed = std::stoi(res.date_cleaned.substr(8, 2));
            }
            else if (boost::regex_match(res.date_cleaned, datetime_regex.YmdTz)) { // Y-m-d T z
                // ISO with time zone
                res.detected_format = "YmdTz";
                res.year_parsed = std::stoi(res.date_cleaned.substr(0, 4));
                res.month_parsed = std::stoi(res.date_cleaned.substr(5, 2));
                res.day_parsed = std::stoi(res.date_cleaned.substr(8, 2));
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
            year_parsed_v.push_back(res.year_parsed.has_value() ? std::to_string(res.year_parsed.value()) : "");
            month_parsed_v.push_back(res.month_parsed.has_value() ? std::to_string(res.month_parsed.value()) : "");
            day_parsed_v.push_back(res.day_parsed.has_value() ? std::to_string(res.day_parsed.value()) : "");
            date_parsed_v.push_back(res.date_parsed);

        }


        // create output dataframe containg parsed date vectors
        return Rcpp::DataFrame::create(
            Rcpp::Named("date_raw") = input_dates,
            // Named("cleaned") = date_cleaned_v, // dont output cleaned dates?
            Rcpp::Named("detected_format") = detected_format_v,
            Rcpp::Named("parsing_error") = parsing_error_v,
            Rcpp::Named("year_parsed") = year_parsed_v,
            Rcpp::Named("month_parsed") = month_parsed_v,
            Rcpp::Named("day_parsed") = day_parsed_v,
            Rcpp::Named("date_parsed") = date_parsed_v
        );

    } catch (std::exception &ex) {
        Rcpp::stop("C++ exception: %s", ex.what());
    } catch (...) {
        Rcpp::stop("Unknown C++ exception");
    }

};



