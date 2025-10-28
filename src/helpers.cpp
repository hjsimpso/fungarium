#include "helpers.h"

std::vector<std::string> convert_r_vec_to_cpp_vec(const Rcpp::CharacterVector& input) {
    std::vector<std::string> result;
    result.reserve(input.size());

    for (unsigned long i = 0; i < static_cast<unsigned long>(input.size()); ++i) {
        if (Rcpp::CharacterVector::is_na(input[i])) {
            result.push_back("");  // NA becomes ""
        } else {
            result.push_back(Rcpp::as<std::string>(input[i]));  // Convert to std::string
        }
    }

    return result;
}

std::map<std::string, std::vector<std::string>> convert_r_df_to_cpp_map(const Rcpp::DataFrame& input) {
    std::map<std::string, std::vector<std::string>> result;

    Rcpp::CharacterVector names = input.names();
    for (int i = 0; i < names.size(); ++i) {
        std::string col_name = Rcpp::as<std::string>(names[i]);
        Rcpp::CharacterVector col_data = input[col_name];
        std::vector<std::string> col_data_cpp = convert_r_vec_to_cpp_vec(col_data);
        result[col_name] = col_data_cpp;
    }

    return result;
}

void print_progress_bar(int current, int total, int bar_width) {
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