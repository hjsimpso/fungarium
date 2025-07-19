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
#include <unordered_set> // For std::unordered_set

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

void isTwoDatesIdentical(const boost::regex& pattern, std::string& cleaned_date, std::optional<std::string>& error) {
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

std::vector<std::optional<std::string>> convert_r_vec_to_cpp_vec(Rcpp::CharacterVector input) {
    std::vector<std::optional<std::string>> result;
    result.reserve(input.size());

    for (unsigned long i = 0; i < input.size(); ++i) {
        if (Rcpp::CharacterVector::is_na(input[i])) {
            result.push_back(std::nullopt);  // NA becomes nullopt
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
    boost::regex dmonthY = boost::regex("^" + fmts.d_fmt + "-" + "(?:" + fmts.month_fmt + ")-" + fmts.Y_fmt + "$", boost::regex_constants::icase);
    boost::regex monthdY = boost::regex("^(?:" + fmts.month_fmt + ")-" + fmts.d_fmt + "-" + fmts.Y_fmt + "$", boost::regex_constants::icase);
    boost::regex Ymonthd = boost::regex("^" + fmts.Y_fmt + "-" + "(?:" + fmts.month_fmt + ")-" + fmts.d_fmt + "$", boost::regex_constants::icase);
    boost::regex Ydmonth = boost::regex("^" + fmts.Y_fmt + "-" + fmts.d_fmt + "-" + "(?:" + fmts.month_fmt + ")$", boost::regex_constants::icase);
    boost::regex YmdT = boost::regex("^" + fmts.Y_fmt + "-" + fmts.m_fmt + "-" + fmts.d_fmt + fmts.T_fmt  +"$");
    boost::regex YmdTz = boost::regex("^" + fmts.Y_fmt + "-" + fmts.m_fmt + "-" + fmts.d_fmt + fmts.T_fmt + fmts.z_fmt + "$");

    boost::regex two_dates_regex = boost::regex("^(" + fmts.Y_fmt + "(?:-" + fmts.m_fmt + "(?:-" + fmts.d_fmt + "(?:" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?)?)?)\\s(" + fmts.Y_fmt + "(?:-" + fmts.m_fmt + "(?:-" + fmts.d_fmt + "(?:" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?)?)?)$");

    // Handles Y, Y-m, Y-m-d, and also supports month names
    boost::regex harmonize_fmt3 = boost::regex("^([0-9]+|(?:" + fmts.month_fmt + "))\\s([0-9]+|(?:" + fmts.month_fmt + "))\\s([0-9]+|(?:" + fmts.month_fmt + "))$", boost::regex_constants::icase);
    boost::regex harmonize_fmt2 = boost::regex("^([0-9]+|(?:" + fmts.month_fmt + "))\\s([0-9]+|(?:" + fmts.month_fmt + "))$", boost::regex_constants::icase);
    boost::regex harmonize_fmt1 = boost::regex("^([0-9]+|(?:" + fmts.month_fmt + "))$", boost::regex_constants::icase);

    // null regex
    boost::regex null_1 = boost::regex("^(" + fmts.Y_fmt + ")-(?:0{1,2})-(?:0{1,2}|" + fmts.d_fmt + ")(?:" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?$");
    boost::regex null_2 = boost::regex("^(?:0{1,2}|" + fmts.d_fmt + ")-(?:0{1,2})-(" + fmts.Y_fmt + ")(?:" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?$");
    boost::regex null_3 = boost::regex("^(" + fmts.Y_fmt + "-" + fmts.m_fmt + ")-(?:0{1,2})(?:" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?$");
    boost::regex null_4 = boost::regex("^(?:0{1,2})-(" + fmts.m_fmt + "-" + fmts.Y_fmt + ")(?:" + fmts.T_fmt + "(?:" + fmts.z_fmt + ")?)?$");
    boost::regex null_5 = boost::regex("^00(?:00)?(?:-00(?:00)?)?$");


};

struct ErrorRegex  {
    boost::regex null_date = boost::regex("^(?:-|(?:0(?:0(?:00)?)?)-(?:0(?:0)?)-(?:0(?:0(?:00)?)?))$");
    boost::regex bad_month = boost::regex("^(?:[0-9]{4}-(?:1[3-9]|[2-9][0-9])(?:$|-))|(?:^[0-9]{1,2}-(?:1[3-9]|[2-9][0-9])-[0-9]{4})");
};


//======================================================================
// Date Cleaning Result Classes
//======================================================================
struct DateResult {
    std::optional<std::string> date_raw;
    std::string date_cleaned;
    std::optional<std::string> detected_format;
    std::optional<std::string> parsing_error;
    std::optional<int> year_parsed;
    std::optional<int> month_parsed;
    std::optional<int> day_parsed;
    std::optional<std::string> date_parsed;

    DateResult(std::optional<std::string>& input_date) : date_raw(input_date), date_cleaned(""), detected_format(std::nullopt), parsing_error(std::nullopt), year_parsed(std::nullopt), month_parsed(std::nullopt), day_parsed(std::nullopt), date_parsed(std::nullopt) {}
    
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
            date_parsed = date_parsed.value() + (month_parsed.value() < 10 ? "0" : "") + std::to_string(month_parsed.value()) + "-";
            date_parsed = date_parsed.value() + (day_parsed.value() < 10 ? "0" : "") + std::to_string(day_parsed.value());
        }
    }

    std::string to_string() {
        std::string result = "DateResult:\n";
        result += "  date_raw: " + (date_raw.has_value() ? date_raw.value() : "NA") + "\n";
        result += "  date_cleaned: " + date_cleaned + "\n";
        result += "  detected_format: " + (detected_format.has_value() ? detected_format.value() : "NA") + "\n";
        result += "  parsing_error: " + (parsing_error.has_value() ? parsing_error.value() : "NA") + "\n";
        result += "  year_parsed: " + (year_parsed.has_value() ? std::to_string(year_parsed.value()) : "NA") + "\n";
        result += "  month_parsed: " + (month_parsed.has_value() ? std::to_string(month_parsed.value()) : "NA") + "\n";
        result += "  day_parsed: " + (day_parsed.has_value() ? std::to_string(day_parsed.value()) : "NA") + "\n";
        result += "  date_parsed: " + (date_parsed.has_value() ? date_parsed.value() : "NA") + "\n";
        return result;
    }
};

class DateResults {
    public:
        std::vector<std::optional<std::string>> date_raw_u; // unique

        std::vector<std::optional<std::string>> date_raw; // not unique
        std::vector<std::optional<std::string>> detected_format;
        std::vector<std::optional<std::string>> parsing_error;
        std::vector<std::optional<int>> year_parsed;
        std::vector<std::optional<int>> month_parsed;
        std::vector<std::optional<int>> day_parsed;
        std::vector<std::optional<std::string>> date_parsed;


        DateResults(std::vector<std::optional<std::string>>& input_dates)
            : detected_format(input_dates.size()), 
            parsing_error(input_dates.size()),
            year_parsed(input_dates.size()),
            month_parsed(input_dates.size()),
            day_parsed(input_dates.size()),
            date_parsed(input_dates.size()){

            date_raw = input_dates;

            std::unordered_set<std::optional<std::string>> seen;
            for (const auto& str : input_dates) {
                if (seen.insert(str).second) {
                    date_raw_u.push_back(str);
                }
            }
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
                r_year_parsed[i] = year_parsed[i].has_value() ? year_parsed[i].value() : NA_INTEGER;
                r_month_parsed[i] = month_parsed[i].has_value() ? month_parsed[i].value() : NA_INTEGER;
                r_day_parsed[i] = day_parsed[i].has_value() ? day_parsed[i].value() : NA_INTEGER;
                r_date_raw[i] = date_raw[i].has_value() ? Rcpp::String(date_raw[i].value()) : NA_STRING;
                r_detected_format[i] = detected_format[i].has_value() ? Rcpp::String(detected_format[i].value()) : NA_STRING;
                r_parsing_error[i] = parsing_error[i].has_value() ? Rcpp::String(parsing_error[i].value()) : NA_STRING;
                r_date_parsed[i] = date_parsed[i].has_value() ? Rcpp::String(date_parsed[i].value()) : NA_STRING;
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

        int add_result(DateResult& date_result){
            for(unsigned long i = 0; i < date_raw.size(); i++){
                if (date_result.date_raw==date_raw[i]){
                    detected_format[i] = date_result.detected_format;
                    parsing_error[i] = date_result.parsing_error;
                    year_parsed[i] = date_result.year_parsed;
                    month_parsed[i] = date_result.month_parsed;
                    day_parsed[i] = date_result.day_parsed;
                    date_parsed[i] = date_result.date_parsed;
                    // Rcpp::Rcout << "Result added for date: " << (date_raw[i].has_value() ? date_raw[i].value() : "NA") << std::endl;
                    // Rcpp::Rcout << "Detected format: " << (detected_format[i].has_value() ? detected_format[i].value() : "NA") << std::endl;
                    // Rcpp::Rcout << "Parsing error: " << (parsing_error[i].has_value() ? parsing_error[i].value() : "NA") << std::endl;
                    // Rcpp::Rcout << "Year parsed: " << (year_parsed[i].has_value() ? std::to_string(year_parsed[i].value()) : "NA") << std::endl;
                    // Rcpp::Rcout << "Month parsed: " << (month_parsed[i].has_value() ? std::to_string(month_parsed[i].value()) : "NA") << std::endl;
                    // Rcpp::Rcout << "Day parsed: " << (day_parsed[i].has_value() ? std::to_string(day_parsed[i].value()) : "NA") << std::endl;
                    // Rcpp::Rcout << "Date parsed: " << (date_parsed[i].has_value() ? date_parsed[i].value() : "NA") << std::endl;
                    // Rcpp::Rcout << "Result added successfully." << std::endl;
                    return i; // return index of added result
                }
            }
            return -1; // return -1 if not found
        }

        int clean_dates(){
            DatetimeRegex datetime_regex;
            ErrorRegex error_regex;
            
            // iterate through each raw date
            for (unsigned long i = 0; i < date_raw_u.size(); i++) {
                // status bar
                print_progress_bar(i, date_raw_u.size());

                // create parsing results container for this date
                DateResult res(date_raw_u[i]);

                // check if date is NA
                if (!res.date_raw.has_value() || res.date_raw == "") {
                    res.parsing_error = "null_date";
                } else {

                    // clean up date strings
                    res.date_cleaned = boost::regex_replace(res.date_raw.value(), boost::regex("[,./\\\\]"), " "); // sub special chars with spaces
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, boost::regex(" {2,}"), " "); // sub 2+ spaces with one space
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, boost::regex("^\\s+|\\s+$"), ""); // remove leading or trailing white space

                    // Harmonize separator: replace space-separated date parts with dashes

                    // Try 3-part (Y m d)
                    if (boost::regex_match(res.date_cleaned, datetime_regex.harmonize_fmt3)) {
                        res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.harmonize_fmt3, "$1-$2-$3");
                    } else {
                        // Try 2-part (Y m)
                        if (boost::regex_match(res.date_cleaned, datetime_regex.harmonize_fmt2)) {
                            res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.harmonize_fmt2, "$1-$2");
                        } else {
                            // Try 1-part (Y)
                            if (boost::regex_match(res.date_cleaned, datetime_regex.harmonize_fmt1)) {
                                res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.harmonize_fmt1, "$1");
                            }
                        }
                    }

                    // handle null values
                    // Remove 00 month or 00-00 month-day (Ymd) - save Y
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.null_1, "$1");
                    // Remove 00 month or 00-00 day-month (dmY) - save Y
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.null_2, "$1");
                    // Remove 00 day (Ymd) - save Ym
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.null_3, "$1");
                    // Remove 00 day (dmY) - save mY
                    res.date_cleaned = boost::regex_replace(res.date_cleaned, datetime_regex.null_4, "$1");
                    // Remove null Y or Ym or mY
                    if (boost::regex_match(res.date_cleaned, datetime_regex.null_5)) {
                        res.date_cleaned = "";
                    }

                    // handle double dates
                    isTwoDatesIdentical(datetime_regex.two_dates_regex, res.date_cleaned, res.parsing_error);

                    // regex search for predefined date formats
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
                }
                // add single date result back to list of dates results
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
        Rcpp::Rcout << "Cleaning dates..." << std::endl;
        std::vector<std::optional<std::string>> input_dates_cpp = convert_r_vec_to_cpp_vec(input_dates);
        Rcpp::Rcout << "Input dates converted to C++ vector." << std::endl;
        DateResults date_results(input_dates_cpp);
        Rcpp::Rcout << "DateResults object created." << std::endl;
        date_results.clean_dates();
        Rcpp::Rcout << "Dates cleaned." << std::endl;
        Rcpp::Rcout << "Returning results as DataFrame." << std::endl;
        return date_results.to_data_frame();
    } catch (std::exception &ex) {
        Rcpp::stop("C++ exception: %s", ex.what());
    } catch (...) {
        Rcpp::stop("Unknown C++ exception");
    }
};



