#pragma once

#include <string>
#include <vector>
#include <map>
#include <Rcpp.h>

std::vector<std::string> convert_r_vec_to_cpp_vec(const Rcpp::CharacterVector& input);

std::map<std::string, std::vector<std::string>> convert_r_df_to_cpp_map(const Rcpp::DataFrame& input);

void print_progress_bar(int current, int total, int bar_width = 50);