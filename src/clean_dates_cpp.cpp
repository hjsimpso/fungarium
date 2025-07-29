//======================================================================
// Imports
//======================================================================

#include <Rcpp.h>
#include <boost/regex.hpp>
#include <string>
#include <vector>
#include <iostream>    // For output
#include <sstream>     // For std::istringstream
#include <optional> // For std::optional
#include <typeinfo> // Required for typeid
#include <unordered_set> // For std::unordered_set
#include <charconv>
#include "date/date.h"

//======================================================================
// Macros
//======================================================================
// #define DEBUG_PRINT(x) Rcpp::Rcout << x << std::endl
#define DEBUG_PRINT(x)

//======================================================================
// ENUM
//======================================================================
enum class DateFormatType {
    Unknown = 0,
    Y,         // Year only
    Ym,        // Year-Month
    mY,        // Month-Year
    Ymd,       // Year-Month-Day
    dmY,       // Day-Month-Year
    dmonthY,   // Day-MonthName-Year
    monthdY,   // MonthName-Day-Year
    Ymonthd,   // Year-MonthName-Day
    Ydmonth,   // Year-Day-MonthName
    Ymonth,   // Year-MonthName
    monthY,   // MonthName-Year
    YmdT,      // Year-Month-Day Time
    YmdTz      // Year-Month-Day Time Zone
};
// Map DateFormatType enum to string representations
const std::map<DateFormatType, std::string> DateFormatMap = {
    {DateFormatType::Unknown, "Unknown"},
    {DateFormatType::Y, "Y"},
    {DateFormatType::Ym, "Ym"},
    {DateFormatType::mY, "mY"},
    {DateFormatType::Ymd, "Ymd"},
    {DateFormatType::dmY, "dmY"},
    {DateFormatType::dmonthY, "dmonthY"},
    {DateFormatType::monthdY, "monthdY"},
    {DateFormatType::Ymonthd, "Ymonthd"},
    {DateFormatType::Ydmonth, "Ydmonth"},
    {DateFormatType::Ymonth, "Ymonth"},
    {DateFormatType::monthY, "monthY"},
    {DateFormatType::YmdT, "YmdT"},
    {DateFormatType::YmdTz, "YmdTz"}
};
//======================================================================
// Clean date helpers
//======================================================================

int get_month_from_name(const std::string& month_raw) {
    static std::map<std::string, int> month_name_map = {
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
    } else {
        throw std::runtime_error("Invalid month name. Could not be mapped to integer value.");
    }
};



std::vector<std::string> convert_r_vec_to_cpp_vec(Rcpp::CharacterVector input) {
    std::vector<std::string> result;
    result.reserve(input.size());

    for (unsigned long i = 0; i < input.size(); ++i) {
        if (Rcpp::CharacterVector::is_na(input[i])) {
            result.push_back("");  // NA becomes ""
        } else {
            result.push_back(Rcpp::as<std::string>(input[i]));  // Convert to std::string
        }
    }

    return result;
}

void print_progress_bar(int current, int total, int bar_width = 50) {
    float progress = static_cast<float>(current) / total;
    int pos = static_cast<int>(bar_width * progress);

    Rcpp::Rcout  << "[";
    for (int i = 0; i < bar_width; ++i) {
        if (i < pos) Rcpp::Rcout  << "=";
        else if (i == pos) Rcpp::Rcout << ">";
        else Rcpp::Rcout << " ";
    }

    Rcpp::Rcout << "] " << int(progress * 100.0) << "%\r";
    Rcpp::Rcout.flush();
}

std::string to_utc_date(std::string_view s) {
    DEBUG_PRINT(s.data());
    if (boost::regex_match(s.data(),boost::regex("^.+Z$", boost::regex_constants::icase | boost::regex_constants::optimize))){ // check for tailing Z (aka UTC time)
        s.remove_suffix(1); // Remove the 'Z' character
        return s.data();
    }

    date::sys_time<std::chrono::seconds> utc_time;
    std::istringstream in(s.data());

    // Parse ISO 8601 with offset (e.g., -05:00)
    in >> date::parse("%FT%T%Ez", utc_time);
    if (in.fail()) {
        throw std::runtime_error("Failed to parse datetime: " + std::string(s.data()));
    }

    // Extract just the date in UTC
    std::ostringstream out;
    out << date::format("%F", utc_time);
    DEBUG_PRINT(out.str());
    return out.str();
}

//======================================================================
// Regex strings
//======================================================================
struct DateTimeFormats{
    std::string Y_fmt = "[0-9]{4}";
    std::string m_fmt = "(?:0?[1-9]|1[0-2])";
    std::string d_fmt = "(?:0?[1-9]|1[0-9]|2[0-9]|3[0-1])";
    std::string T_fmt = "[0-9]{2}:[0-9]{2}:[0-9]{2}";
    std::string z_fmt = "(?:Z|[\\+-][0-9]{2}:?[0-9]{2})";
    std::string month_fmt = "(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)";
};

struct DatetimeRegex {
    DateTimeFormats fmts;

    boost::regex Y = boost::regex("^" + fmts.Y_fmt + "$", boost::regex_constants::optimize);
    boost::regex Ym = boost::regex("^" + fmts.Y_fmt + "-" + fmts.m_fmt + "$", boost::regex_constants::optimize);
    boost::regex mY = boost::regex("^" + fmts.m_fmt + "-" + fmts.Y_fmt + "$", boost::regex_constants::optimize);
    boost::regex Ymd = boost::regex("^" + fmts.Y_fmt + "-" + fmts.m_fmt + "-" + fmts.d_fmt + "$", boost::regex_constants::optimize);
    boost::regex dmY = boost::regex("^" + fmts.d_fmt + "-" + fmts.m_fmt + "-" + fmts.Y_fmt + "$", boost::regex_constants::optimize);
    boost::regex dmonthY = boost::regex("^" + fmts.d_fmt + "-" + fmts.month_fmt + "-" + fmts.Y_fmt + "$", boost::regex_constants::icase | boost::regex_constants::optimize);
    boost::regex monthdY = boost::regex("^" + fmts.month_fmt + "-" + fmts.d_fmt + "-" + fmts.Y_fmt + "$", boost::regex_constants::icase | boost::regex_constants::optimize);
    boost::regex Ymonthd = boost::regex("^" + fmts.Y_fmt + "-" + fmts.month_fmt + "-" + fmts.d_fmt + "$", boost::regex_constants::icase | boost::regex_constants::optimize);
    boost::regex Ydmonth = boost::regex("^" + fmts.Y_fmt + "-" + fmts.d_fmt + "-" + fmts.month_fmt + "$", boost::regex_constants::icase | boost::regex_constants::optimize);
    boost::regex monthY = boost::regex("^" + fmts.month_fmt + "-" + fmts.Y_fmt + "$", boost::regex_constants::icase | boost::regex_constants::optimize);
    boost::regex Ymonth = boost::regex("^" + fmts.Y_fmt + "-" + fmts.month_fmt + "$", boost::regex_constants::icase | boost::regex_constants::optimize);
    boost::regex YmdT = boost::regex("^" + fmts.Y_fmt + "-" + fmts.m_fmt + "-" + fmts.d_fmt + "[T\\s]" + fmts.T_fmt  +"$", boost::regex_constants::optimize);
    boost::regex YmdTz = boost::regex("^" + fmts.Y_fmt + "-" + fmts.m_fmt + "-" + fmts.d_fmt + "[T\\s]" + fmts.T_fmt + fmts.z_fmt + "$", boost::regex_constants::optimize);

    // finds double date strings
    boost::regex two_dates_regex = boost::regex("^(" + fmts.Y_fmt + "(?:-" + fmts.m_fmt + "(?:-" + fmts.d_fmt + "(?:[T\\s]" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?)?)?)[\\s-](" + fmts.Y_fmt + "(?:-" + fmts.m_fmt + "(?:-" + fmts.d_fmt + "(?:[T\\s]" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?)?)?)$", boost::regex_constants::icase | boost::regex_constants::optimize);

    // Finds spaces in date format - used to harmonize with dash separator in dates and T separator in time
    boost::regex harmonize_ymd = boost::regex("^([0-9]+|" + fmts.month_fmt + ")\\s([0-9]+|" + fmts.month_fmt + ")\\s([0-9]+|" + fmts.month_fmt + ")$", boost::regex_constants::icase | boost::regex_constants::optimize); // for Ymd etc.
    boost::regex harmonize_ym = boost::regex("^([0-9]+|" + fmts.month_fmt + ")\\s([0-9]+|" + fmts.month_fmt + ")$", boost::regex_constants::icase | boost::regex_constants::optimize); // for Ym etc.
    boost::regex harmonize_ymdt = boost::regex("^([0-9]+|" + fmts.month_fmt + ")\\s([0-9]+|" + fmts.month_fmt + ")\\s([0-9]+|" + fmts.month_fmt + ")[T\\s](" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)$", boost::regex_constants::icase | boost::regex_constants::optimize); // for YmdT or YmdTz

    // for cleaning dull date values
    boost::regex null_1 = boost::regex("^(" + fmts.Y_fmt + ")-(?:0{1,2})-(?:0{1,2}|" + fmts.d_fmt + ")(?:[T\\s]" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?$", boost::regex_constants::icase | boost::regex_constants::optimize); // null month, capture year
    boost::regex null_2 = boost::regex("^(?:0{1,2}|" + fmts.d_fmt + ")-(?:0{1,2})-(" + fmts.Y_fmt + ")(?:[T\\s]" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?$", boost::regex_constants::icase | boost::regex_constants::optimize); // null month, capture year
    boost::regex null_3 = boost::regex("^(" + fmts.Y_fmt + "-" + fmts.m_fmt + ")-(?:0{1,2})(?:[T\\s]" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?$", boost::regex_constants::icase | boost::regex_constants::optimize); // null day, capture year-month
    boost::regex null_4 = boost::regex("^(?:0{1,2})-(" + fmts.m_fmt + "-" + fmts.Y_fmt + ")(?:[T\\s]" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?$", boost::regex_constants::icase | boost::regex_constants::optimize); // null day, capture year-month
    boost::regex null_5 = boost::regex("^00(?:00)?(?:-00(?:00)?)?$", boost::regex_constants::optimize); // null year or year-month or month-year, capture nothing ('useless' date)
    boost::regex null_6 = boost::regex("^0000-" + fmts.m_fmt + "-" + fmts.d_fmt + "(?:[T\\s]" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?$", boost::regex_constants::icase | boost::regex_constants::optimize); // null year, capture nothing
    boost::regex null_7 = boost::regex("^" + fmts.d_fmt + "-" + fmts.m_fmt + "-0000(?:[T\\s]" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?$", boost::regex_constants::icase | boost::regex_constants::optimize); // null year, capture nothing

    // for cleaning spec character and whitespace
    boost::regex spec_char = boost::regex("[,./\\\\]", boost::regex_constants::optimize); // finds spec chars
    boost::regex double_space = boost::regex(" {2,}", boost::regex_constants::optimize); // finds 2+ spaces
    boost::regex extra_whitespace = boost::regex("^\\s+|\\s+$", boost::regex_constants::optimize); // finds leading and tailing whitespace
};

struct ErrorRegex  {
    // boost::regex null_date = boost::regex("^(?:-|(?:0(?:0(?:00)?)?)-(?:0(?:0)?)-(?:0(?:0(?:00)?)?))$", boost::regex_constants::optimize);
    boost::regex bad_month = boost::regex("^(?:[0-9]{4}-(?:1[3-9]|[2-9][0-9])(?:$|-.*))|(?:^(?:[0-9]{1,2}-)?(?:1[3-9]|[2-9][0-9])-[0-9]{4}).*$", boost::regex_constants::optimize);
};


//======================================================================
// Date Cleaning Result Classes
//======================================================================
struct DateResult {
    std::string date_raw;
    std::string date_cleaned;
    std::string detected_format;
    std::string parsing_error;
    int year_parsed = 0;
    int month_parsed = 0;
    int day_parsed = 0;
    std::string date_parsed;

    DateResult(const std::string& input_date) : date_raw(input_date), date_cleaned(""), detected_format(""), parsing_error(""), year_parsed(0), month_parsed(0), day_parsed(0), date_parsed("") {}
    
    // create date string (YYYY-MM-DD) from year, month, day integers
    void format_date() {
        // Format date as YYYY-MM-DD
        if (year_parsed>0&&month_parsed>0&&day_parsed>0){
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
            date_parsed = year_prefix + std::to_string(year_parsed) + "-";
            date_parsed = date_parsed + (month_parsed < 10 ? "0" : "") + std::to_string(month_parsed) + "-";
            date_parsed = date_parsed + (day_parsed < 10 ? "0" : "") + std::to_string(day_parsed);
        }
    }

    std::string to_string() {
        std::string result = "DateResult:\n";
        result += "  date_raw: " + (!date_raw.empty() ? date_raw : "NA") + "\n";
        result += "  date_cleaned: " + date_cleaned + "\n";
        result += "  detected_format: " + (!detected_format.empty() ? detected_format : "NA") + "\n";
        result += "  parsing_error: " + (!parsing_error.empty() ? parsing_error : "NA") + "\n";
        result += "  year_parsed: " + (year_parsed>0 ? std::to_string(year_parsed) : "NA") + "\n";
        result += "  month_parsed: " + (month_parsed>0 ? std::to_string(month_parsed) : "NA") + "\n";
        result += "  day_parsed: " + (day_parsed>0 ? std::to_string(day_parsed) : "NA") + "\n";
        result += "  date_parsed: " + (!date_parsed.empty() ? parsing_error : "NA") + "\n";
        return result;
    }

    int check_dup_dates(const boost::regex& dup_date_pattern) {
        if (boost::regex_match(date_cleaned, dup_date_pattern)) {
            std::string d1;
            std::string d2;
            d1 = boost::regex_replace(date_cleaned, dup_date_pattern, "$1");
            DEBUG_PRINT(d1);
            d2 = boost::regex_replace(date_cleaned, dup_date_pattern, "$2");
            DEBUG_PRINT(d2);
            if (d1 == d2) {
                date_cleaned = d1;
                return 1;
            } else {
                return -1;
            }
        }
        return 0;
    }

    void parse_date(DateFormatType detected_format){
        // Faster parsing using std::string_view and manual indexing (no vector allocation)
        this->detected_format = DateFormatMap.at(detected_format);

        std::string_view date_str(date_cleaned);
        static std::size_t first_dash = std::string_view::npos;
        static std::size_t second_dash = std::string_view::npos;
        static std::size_t time_delimter = std::string_view::npos;
        std::string_view year_substr;
        std::string_view month_substr;
        std::string_view day_substr;

        switch (detected_format) {
            case DateFormatType::Y:
                year_substr = date_cleaned;        
                break;
            case DateFormatType::Ym:
                // year, month (1996-01)
                first_dash = date_str.find('-');

                if (first_dash != std::string_view::npos) {
                    year_substr = date_str.substr(0, first_dash);
                    month_substr = date_str.substr(first_dash + 1);
                }                     
                break;
            case DateFormatType::mY:
                // month, year (01-1996)
                first_dash = date_str.find('-');

                if (first_dash != std::string_view::npos) {
                    month_substr = date_str.substr(0, first_dash);
                    year_substr = date_str.substr(first_dash + 1);
                }                   
                break;
            case DateFormatType::Ymd:
                // year, month, day (1996-01-05)
                first_dash = date_str.find('-');
                second_dash = date_str.find('-', first_dash + 1);

                if (first_dash != std::string_view::npos && second_dash != std::string_view::npos) {
                    year_substr = date_str.substr(0, first_dash);
                    month_substr = date_str.substr(first_dash + 1, second_dash - first_dash - 1);
                    day_substr = date_str.substr(second_dash + 1);
                }                
                break;
            case DateFormatType::dmY:
                // day, month, year (05-01-1996)
                first_dash = date_str.find('-');
                second_dash = date_str.find('-', first_dash + 1);

                if (first_dash != std::string_view::npos && second_dash != std::string_view::npos) { 
                    day_substr = date_str.substr(0, first_dash);
                    month_substr = date_str.substr(first_dash + 1, second_dash - first_dash - 1);
                    year_substr = date_str.substr(second_dash + 1);
                }                  
                break;
            case DateFormatType::Ydmonth:
                // year, month, day (1996-05-Jan)
                first_dash = date_str.find('-');
                second_dash = date_str.find('-', first_dash + 1);

                if (first_dash != std::string_view::npos && second_dash != std::string_view::npos) {
                    year_substr = date_str.substr(0, first_dash);
                    day_substr = date_str.substr(first_dash + 1, second_dash - first_dash - 1);
                    month_substr = date_str.substr(second_dash + 1);

                    try {
                        month_parsed = get_month_from_name(std::string(month_substr));
                    } catch (std::exception &ex){
                        parsing_error = "undefined_error";
                        return;
                    }
                }                  
                break;
            case DateFormatType::dmonthY:
                // day, month, year (05-Jan-1996)
                first_dash = date_str.find('-');
                second_dash = date_str.find('-', first_dash + 1);

                if (first_dash != std::string_view::npos && second_dash != std::string_view::npos) {     
                    day_substr = date_str.substr(0, first_dash);
                    month_substr = date_str.substr(first_dash + 1, second_dash - first_dash - 1);
                    year_substr = date_str.substr(second_dash + 1);

                    try {
                        month_parsed = get_month_from_name(std::string(month_substr));
                    } catch (std::exception &ex){
                        parsing_error = "undefined_error";
                        return;
                    } 
                }               
                break;
            case DateFormatType::monthdY:
                // month, day, year (Jan-05-1996)
                first_dash = date_str.find('-');
                second_dash = date_str.find('-', first_dash + 1);

                if (first_dash != std::string_view::npos && second_dash != std::string_view::npos) {
                    month_substr = date_str.substr(0, first_dash);
                    day_substr = date_str.substr(first_dash + 1, second_dash - first_dash - 1);
                    year_substr = date_str.substr(second_dash + 1);

                    try {
                        month_parsed = get_month_from_name(std::string(month_substr));
                    } catch (std::exception &ex){
                        parsing_error = "undefined_error";
                        return;
                    }
                }
                break;
            case DateFormatType::Ymonthd:
                // year, month, day (1996-Jan-05)
                first_dash = date_str.find('-');
                second_dash = date_str.find('-', first_dash + 1);

                if (first_dash != std::string_view::npos && second_dash != std::string_view::npos) { 
                    year_substr= date_str.substr(0, first_dash);
                    month_substr = date_str.substr(first_dash + 1, second_dash - first_dash - 1);
                    day_substr = date_str.substr(second_dash + 1);
                    try {
                        month_parsed = get_month_from_name(std::string(month_substr));
                    } catch (std::exception &ex){
                        parsing_error = "undefined_error";
                        return;
                    }
                }
                break;
            case DateFormatType::Ymonth:
                // year, month (1996-Jan)
                first_dash = date_str.find('-');

                if (first_dash != std::string_view::npos) {
                    year_substr = date_str.substr(0, first_dash);
                    month_substr = date_str.substr(first_dash + 1);

                    try {
                        month_parsed = get_month_from_name(std::string(month_substr));
                    } catch (std::exception &ex){
                        parsing_error = "undefined_error";
                        return;
                    }
                }                 
                break;
            case DateFormatType::monthY:
                // month, year (Jan-1996)
                first_dash = date_str.find('-');

                if (first_dash != std::string_view::npos) {
                    month_substr = date_str.substr(0, first_dash);
                    year_substr = date_str.substr(first_dash + 1);

                    try {
                        month_parsed = get_month_from_name(std::string(month_substr));
                    } catch (std::exception &ex){
                        parsing_error = "undefined_error";
                        return;
                    }
                }                 
                break;
            case DateFormatType::YmdT:
                // year, month, day, time (1996-01-05T00:00:00)
                first_dash = date_str.find('-');
                second_dash = date_str.find('-', first_dash + 1);
                time_delimter = date_str.find('T');

                if (first_dash != std::string_view::npos && second_dash != std::string_view::npos && time_delimter != std::string_view::npos) {
                    year_substr = date_str.substr(0, first_dash);
                    month_substr = date_str.substr(first_dash + 1, second_dash - first_dash - 1);
                    day_substr = date_str.substr(second_dash + 1, time_delimter - second_dash - 1);
                }                 
                break;
            case DateFormatType::YmdTz:
                // year, month, day, time, zone (1996-01-05T00:00:00-00:00)
                try {
                    std::string_view harmonized_utc = to_utc_date(date_str);
                    first_dash = harmonized_utc.find('-');
                    second_dash = harmonized_utc.find('-', first_dash + 1);

                    if (first_dash != std::string_view::npos && second_dash != std::string_view::npos) {
                        year_substr = harmonized_utc.substr(0, first_dash);
                        month_substr = harmonized_utc.substr(first_dash + 1, second_dash - first_dash - 1);
                        day_substr = harmonized_utc.substr(second_dash + 1, time_delimter - second_dash - 1);
                    } 
                } catch (std::exception &ex){
                    parsing_error = "undefined_error";
                    return;
                }
                break;
            default:
                parsing_error = "undefined_error";
                break; // Optional for the default case if it's the last statement
        }
        // parse year month day to results container
        if (!year_substr.empty()){
            std::from_chars(year_substr.data(), year_substr.data() + year_substr.size(), year_parsed);
        }
        if (month_parsed == 0) { // if month was text then month has already been parsed to int, so this section can be skipped
            if (!month_substr.empty()){
                std::from_chars(month_substr.data(), month_substr.data() + month_substr.size(), month_parsed);
            }
        }

        if (!day_substr.empty()){ // check that day_subtr is not empty and that month is not null
            std::from_chars(day_substr.data(), day_substr.data() + day_substr.size(), day_parsed);
        }
        // create date string from year, month, day ints
        format_date();

    }
};

class DateResults {
    public:
        std::vector<std::string> date_raw_u; // unique
        std::vector<std::string> date_raw; // not unique
        std::vector<std::string> detected_format;
        std::vector<std::string> parsing_error;
        std::vector<int> year_parsed;
        std::vector<int> month_parsed;
        std::vector<int> day_parsed;
        std::vector<std::string> date_parsed;

        // Map from unique date string to all indices in date_raw where it occurs
        std::unordered_map<std::string, std::vector<std::size_t>> date_raw_indices;

        DateResults(std::vector<std::string>& input_dates)
            : detected_format(input_dates.size()), 
            parsing_error(input_dates.size()),
            year_parsed(input_dates.size()),
            month_parsed(input_dates.size()),
            day_parsed(input_dates.size()),
            date_parsed(input_dates.size()){

            date_raw = input_dates;
            
            // make list of unique dates for faster processing
            std::unordered_set<std::string> seen;
            for (std::size_t i = 0; i < input_dates.size(); ++i) {
            const auto& str = input_dates[i];
            if (seen.insert(str).second) {
                date_raw_u.push_back(str);
            }
            date_raw_indices[str].push_back(i);
            }
        }

        // Modified add_result: assign to all occurrences of the date in date_raw
        int add_result(DateResult& date_result){
            auto it = date_raw_indices.find(date_result.date_raw);
            if (it == date_raw_indices.end()) return -1;

            for (std::size_t i : it->second) {
                detected_format[i] = date_result.detected_format;
                parsing_error[i] = date_result.parsing_error;
                year_parsed[i] = date_result.year_parsed;
                month_parsed[i] = date_result.month_parsed;
                day_parsed[i] = date_result.day_parsed;
                date_parsed[i] = date_result.date_parsed;
            }
            return 0;
        }

        Rcpp::DataFrame to_data_frame(){
            unsigned long n = date_raw.size();
            Rcpp::IntegerVector r_year_parsed(n);
            Rcpp::IntegerVector r_month_parsed(n);
            Rcpp::IntegerVector r_day_parsed(n);
            Rcpp::CharacterVector r_date_raw(n);
            Rcpp::CharacterVector r_detected_format(n);
            Rcpp::CharacterVector r_parsing_error(n);
            Rcpp::CharacterVector r_date_parsed(n);

            for (unsigned long i = 0; i < n; ++i) {
                r_year_parsed[i] = year_parsed[i]>0 ? year_parsed[i] : NA_INTEGER;
                r_month_parsed[i] = month_parsed[i]>0 ? month_parsed[i] : NA_INTEGER;
                r_day_parsed[i] = day_parsed[i]>0 ? day_parsed[i] : NA_INTEGER;
                r_date_raw[i] = !date_raw[i].empty() ? Rcpp::String(date_raw[i]) : NA_STRING;
                r_detected_format[i] = !detected_format[i].empty() ? Rcpp::String(detected_format[i]) : NA_STRING;
                r_parsing_error[i] = !parsing_error[i].empty() ? Rcpp::String(parsing_error[i]) : NA_STRING;
                r_date_parsed[i] = !date_parsed[i].empty() ? Rcpp::String(date_parsed[i]) : NA_STRING;
            }
            return Rcpp::DataFrame::create(
                Rcpp::Named("date_raw") = r_date_raw,
                Rcpp::Named("detected_format") = r_detected_format,
                Rcpp::Named("parsing_error") = r_parsing_error,
                Rcpp::Named("year_parsed") = r_year_parsed,
                Rcpp::Named("month_parsed") = r_month_parsed,
                Rcpp::Named("day_parsed") = r_day_parsed,
                Rcpp::Named("date_parsed") = r_date_parsed
            );
        };


        int clean_dates(){
            // Optimization: Move regex and string allocations outside the loop
            static DatetimeRegex datetime_regex;
            static ErrorRegex error_regex;

            // Use references to avoid repeated lookups
            auto& date_raw_u_ref = date_raw_u;

            // Iterate through each unique raw date
            for (unsigned long i = 0; i < date_raw_u_ref.size(); i++) {
                const std::string& raw = date_raw_u_ref[i];
                DateResult res(raw);

                // Check if date is NA
                if (res.date_raw.empty()) {
                    res.parsing_error = "null_date";
                } else {
                    // Clean up date strings (minimize string copies)
                    std::string tmp = boost::regex_replace(res.date_raw, datetime_regex.spec_char, " "); // sub special chars with spaces
                    tmp = boost::regex_replace(tmp, datetime_regex.double_space, " "); // sub special chars with spaces
                    tmp = boost::regex_replace(tmp, datetime_regex.extra_whitespace, ""); // remove leading or trailing white space
                    res.date_cleaned = std::move(tmp);
                    DEBUG_PRINT(res.date_cleaned);

                    // Harmonize separator: replace space-separated date parts with dashes and space separated time parts with T
                    if (boost::regex_match(res.date_cleaned, datetime_regex.harmonize_ymd)) {
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.harmonize_ymd, "$1-$2-$3"); // Ymd
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.harmonize_ymdt)) {
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.harmonize_ymdt, "$1-$2-$3T$4"); // YmdT
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.harmonize_ym)) {
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.harmonize_ym, "$1-$2"); // Ym
                    }
                    DEBUG_PRINT(res.date_cleaned);

                    // Handle null values
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.null_1, "$1"); // Remove 00 month or 00-00 month-day (Ymd) - save Y
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.null_2, "$1"); // Remove 00 month or 00-00 day-month (dmY) - save Y
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.null_3, "$1"); // Remove 00 day (Ymd) - save Ym
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.null_4, "$1"); // Remove 00 day (dmY) - save mY
                    if (boost::regex_match(res.date_cleaned, datetime_regex.null_5) ||
                        boost::regex_match(res.date_cleaned, datetime_regex.null_6) ||
                        boost::regex_match(res.date_cleaned, datetime_regex.null_7)) {
                        res.date_cleaned = "";
                    }
                    DEBUG_PRINT(res.date_cleaned);

                    // Regex search for predefined date formats (order by expected frequency for faster matching)
                    if (res.date_cleaned.empty()) {
                        res.parsing_error = "null_date";
                    } else if(res.check_dup_dates(datetime_regex.two_dates_regex)<0){                     // Handle double dates
                        res.parsing_error = "two_dates";
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.YmdTz)) {
                    res.parse_date(DateFormatType::YmdTz);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.Ymd)) {
                    res.parse_date(DateFormatType::Ymd);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.dmonthY)) {
                    res.parse_date(DateFormatType::dmonthY);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.Ym)) {
                    res.parse_date(DateFormatType::Ym);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.Ymonthd)) {
                    res.parse_date(DateFormatType::Ymonthd);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.Y)) {
                    res.parse_date(DateFormatType::Y);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.dmY)) {
                    res.parse_date(DateFormatType::dmY);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.Ymonth)) {
                    res.parse_date(DateFormatType::Ymonth);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.monthY)) {
                    res.parse_date(DateFormatType::monthY);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.YmdT)) {
                    res.parse_date(DateFormatType::YmdT);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.monthdY)) {
                    res.parse_date(DateFormatType::monthdY);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.mY)) {
                    res.parse_date(DateFormatType::mY);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.Ymonthd)) {
                    res.parse_date(DateFormatType::Ymonthd);
                    } else if (boost::regex_match(res.date_cleaned, datetime_regex.Ydmonth)) {
                    res.parse_date(DateFormatType::Ydmonth);
                    } else if (boost::regex_match(res.date_cleaned, error_regex.bad_month)) {
                        res.parsing_error = "bad_month";
                    } else {
                        res.parsing_error = "undefined_error";
                    }
                }
                // Add single date result back to list of dates results
                DEBUG_PRINT(res.to_string());
                add_result(res);
            }
            return 0;
        }
};


//======================================================================
// Clean dates function
//======================================================================
// [[Rcpp::depends(BH)]]
// [[Rcpp::export]]
Rcpp::DataFrame clean_dates_cpp(const Rcpp::CharacterVector& input_dates) {
    try{
        DEBUG_PRINT("Cleaning dates...");
        std::vector<std::string> input_dates_cpp = convert_r_vec_to_cpp_vec(input_dates);
        DateResults date_results(input_dates_cpp);
        date_results.clean_dates();
        DEBUG_PRINT("Dates cleaned.");
        return date_results.to_data_frame();
    } catch (std::exception &ex) {
        Rcpp::stop("C++ exception: %s", ex.what());
    } catch (...) {
        Rcpp::stop("Unknown C++ exception");
    }
};



