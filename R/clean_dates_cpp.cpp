#include <Rcpp.h>
#include <regex>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <cctype>
#include <ctime>

using namespace Rcpp;
// using namespace std;

// Your helper functions like get_month_from_name() go here

// [[Rcpp::export]]
DataFrame clean_dates_cpp(const std::vector<std::string>& input_dates) {
  // (Your clean_dates implementation from above goes here)

  // Placeholder: outputs for demonstration

  // create containers for output data columns
  std::vector<std::string> date_cleaned_v;
  std::vector<std::string> detected_format_v;
  std::vector<std::string> parsing_error_v;
  std::vector<std::string> year_parsed_v;
  std::vector<std::string> month_parsed_v;
  std::vector<std::string> day_parsed_v;
  std::vector<std::string> date_parsed_v;


  // create containers for output data column elements
  std::string detected_format;
  std::string parsing_error;
  std::string year_parsed;
  std::string month_parsed;
  std::string day_parsed;
  std::string date_parsed;

  // create regex search strings
  std::regex Y_fmt("^\\d{4}$");
  std::regex Ym_fmt("^\\d{4}-\\d{1,2}$");
  std::regex mY_fmt("^\\d{1,2}-\\d{4}$");
  std::regex Ymd_fmt("^\\d{4}-\\d{1,2}-\\d{1,2}$");
  std::regex dmY_fmt("^\\d{1,2}-\\d{1,2}-\\d{4}$");
  std::regex two_dates_fmt(R"(^(\d{4}-\d{2}-\d{2})(?:T\d{2}:\d{2}:\d{2})?(?:Z|[\+\-]\d{2}:?\d{2})?\s+(\d{4}-\d{2}-\d{2})(?:T\d{2}:\d{2}:\d{2})?(?:Z|[\+\-]\d{2}:?\d{2})?$)");





  for (const std::string& date : input_dates) {
    // Use your full parsing logic here

    // Dummy example logic:
    date_cleaned_v.push_back(date);                   // Replace with parsed value
    detected_format_v.push_back(detected_format);     // Replace with actual detected format
    parsing_error_v.push_back(parsing_error);          // Replace with actual error if any
    year_parsed_v.push_back(year_parsed);              // Replace with actual year parsed
    month_parsed_v.push_back(month_parsed);            // Replace with actual month parsed
    day_parsed_v.push_back(day_parsed);                // Replace with actual day parsed
    date_parsed_v.push_back(date_parsed);              // Replace with actual date parsed
  }

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


bool isTwoDatesIdentical(const std::string& input, const std::regex& pattern, std::string& cleaned_date, std::string& error) {
    std::smatch match;
    if (std::regex_match(input, match, pattern)) {
        std::string d1 = match[1];
        std::string d2 = match[6];
        if (d1 == d2) {
            cleaned_date = d1;
            return true;
        } else {
            cleaned_date = input;
            error = "two_dates";
            return false;
        }
    }
    return false;
}

optional<sys_days> parse_date(const std::string& date_str, const std::string& format) {
    std::istringstream in{date_str};
    sys_days tp;
    in >> parse(format, tp);
    if (!in.fail()) return tp;
    return nullopt;
}

vector<DateResult> clean_dates(const vector<string>& dates) {
    vector<DateResult> results;


    for (const auto& raw_date : dates) {
        DateResult res;
        res.date_raw = raw_date;

        string cleaned = regex_replace(raw_date, regex("[./\\\\]"), " ");
        cleaned = regex_replace(cleaned, regex(" {2,}"), " ");
        cleaned = regex_replace(cleaned, regex("^\\s+|\\s+$"), "");

        string detected_format;
        string error;
        optional<sys_days> parsed_date;

        string updated_date = cleaned;
        bool is_duplicate = isTwoDatesIdentical(cleaned, two_dates_fmt, updated_date, error);
        cleaned = updated_date;

        if (cleaned.empty()) {
            res.parsing_error = "null_date";
        } else if (regex_match(cleaned, year_only)) {
            detected_format = "Y";
            parsed_date = parse_date(cleaned, "%Y");
        } else if (regex_match(cleaned, ym_fmt)) {
            detected_format = "Ym";
            parsed_date = parse_date(cleaned + "-01", "%Y-%m-%d");
        } else if (regex_match(cleaned, mY_fmt)) {
            detected_format = "mY";
            stringstream ss(cleaned);
            string m, y;
            getline(ss, m, '-');
            getline(ss, y);
            parsed_date = parse_date(y + "-" + m + "-01", "%Y-%m-%d");
        } else if (regex_match(cleaned, ymd_fmt)) {
            detected_format = "Ymd";
            parsed_date = parse_date(cleaned, "%Y-%m-%d");
        } else if (regex_match(cleaned, dmy_fmt)) {
            detected_format = "dmY";
            stringstream ss(cleaned);
            string d, m, y;
            getline(ss, d, '-');
            getline(ss, m, '-');
            getline(ss, y);
            parsed_date = parse_date(y + "-" + m + "-" + d, "%Y-%m-%d");
        } else if (regex_match(cleaned, regex("^\\d{1,2}-[A-Za-z]+-\\d{4}$"))) {
          // dmonthY: 13-Jan-2024
          vector<string> parts;
          stringstream ss(cleaned);
          string segment;
          while (getline(ss, segment, '-')) parts.push_back(segment);
          int d = stoi(parts[0]);
          int m = get_month_from_name(parts[1]);
          int y = stoi(parts[2]);
          if (m > 0) {
              detected_format = "dmonthY";
              parsed_date = parse_date(to_string(y) + "-" + to_string(m) + "-" + to_string(d), "%Y-%m-%d");
          }
        }
        else if (regex_match(cleaned, regex("^[A-Za-z]+ \\d{1,2} \\d{4}$"))) {
          // monthdY: Jan 13 2024
          vector<string> parts;
          stringstream ss(cleaned);
          string month, day, year;
          ss >> month >> day >> year;
          int m = get_month_from_name(month);
          if (m > 0) {
              detected_format = "monthdY";
              parsed_date = parse_date(year + "-" + to_string(m) + "-" + day, "%Y-%m-%d");
          }
        }
        else if (regex_match(cleaned, regex("^\\d{4} [A-Za-z]+ \\d{1,2}$"))) {
          // Ymonthd: 2024 January 13
          vector<string> parts;
          stringstream ss(cleaned);
          string year, month, day;
          ss >> year >> month >> day;
          int m = get_month_from_name(month);
          if (m > 0) {
              detected_format = "Ymonthd";
              parsed_date = parse_date(year + "-" + to_string(m) + "-" + day, "%Y-%m-%d");
          }
        }
        else if (regex_match(cleaned, regex("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[\\+\\-]\\d{2}:?\\d{2})?$"))) {
            // ISO with time zone
            detected_format = "Ymd HMS TZ";
            parsed_date = parse_date(cleaned.substr(0, 10), "%Y-%m-%d");
        }
        else {
            error = "undefined_error";
        }

        if (!detected_format.empty()) res.detected_format = detected_format;
        if (!error.empty()) res.parsing_error = error;

        if (parsed_date.has_value()) {
            auto ymd = year_month_day{parsed_date.value()};
            res.date_parsed = format("%F", parsed_date.value());
            res.year_parsed = int(ymd.year());
            res.month_parsed = unsigned(ymd.month());
            res.day_parsed = unsigned(ymd.day());
        }

        results.push_back(res);
    }

    return results;
}