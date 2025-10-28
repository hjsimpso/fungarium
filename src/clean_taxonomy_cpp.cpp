//======================================================================
// Imports
//======================================================================

// STL
#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <unordered_map>
#include <sstream>

//Rcpp
#include <Rcpp.h>

// Vendor
#include <boost/regex.hpp>
#include <rapidfuzz/fuzz.hpp>

// fungarium
#include "helpers.h"

//======================================================================
// Macros
//======================================================================
#define DEBUG_PRINT(x) Rcpp::Rcout << x << std::endl
// #define DEBUG_PRINT(x)

//======================================================================
// Types
//======================================================================
typedef std::map<std::string, std::vector<std::string>> ColData;
typedef std::string COLAcceptedTaxonID;
typedef std::string COLScientificNameMatch;
typedef std::string COLMatchType;

//======================================================================
// Matching structs
//======================================================================
struct COLExactMatch{
    std::string accepted_taxon_id;
    std::string match_type;
    std::string sci_name;
    std::string error = "";
};
struct COLFuzzyMatch{
    std::string accepted_taxon_id;
    double match_score;
    std::string match_type;
    std::string sci_name;
};

//======================================================================
// Constants
//======================================================================
const boost::regex species_hypothesis_regex = boost::regex("^SH[0-9]+\\.[0-9]+FU$",boost::regex_constants::optimize);

//======================================================================
// Helpers
//======================================================================
ColData read_col_data_file(const std::string& filename) {
    ColData dataMap;
    std::ifstream file(filename);

    if (!file.is_open()) {
        // Handle error: file could not be opened
        return dataMap;
    }

    std::string header_line;
    if (!std::getline(file, header_line)) {
        // Handle error: empty file
        return dataMap;
    }

    // Parse header to get field names
    std::vector<std::string> field_names;
    std::stringstream header_ss(header_line);
    std::string field;
    while (std::getline(header_ss, field, '\t')) {
        field_names.push_back(field);
        dataMap[field] = std::vector<std::string>();
    }

    // Read data lines
    std::string line;
    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string value;
        std::size_t col_idx = 0;
        while (std::getline(ss, value, '\t')) {
            if (col_idx < field_names.size()) {
                dataMap[field_names[col_idx]].push_back(value);
            }
            ++col_idx;
        }
        // Fill missing columns with empty string if line is short
        while (col_idx < field_names.size()) {
            dataMap[field_names[col_idx]].push_back("");
            ++col_idx;
        }
    }

    file.close();
    return dataMap;
}


class COLIndex {
    public:
        std::unordered_map<std::string, std::vector<size_t>> sci_full_to_idx;
        std::unordered_map<std::string, std::vector<size_t>> sci_partial_to_idx;
        std::unordered_map<std::string, size_t> taxon_id_to_idx;

        std::vector<std::string> full_names;
        std::vector<std::string> partial_names;

        COLIndex(const ColData& col_data) {
            // Check required columns exist
            std::vector<std::string> required_cols = {
                "dwc:scientificName", "dwc:genericName", "dwc:specificEpithet", "dwc:infraspecificEpithet", "dwc:taxonID"
            };
            for (const auto& col : required_cols) {
                if (col_data.find(col) == col_data.end()) {
                    throw std::runtime_error("Missing required column in COL data: " + col);
                }
            }
        
            const auto& sci_names = col_data.at("dwc:scientificName");
            const auto& genus     = col_data.at("dwc:genericName");
            const auto& species   = col_data.at("dwc:specificEpithet");
            const auto& infra     = col_data.at("dwc:infraspecificEpithet");
            const auto& taxon_ids = col_data.at("dwc:taxonID");
        
            size_t n = sci_names.size();
            full_names.reserve(n);
            partial_names.reserve(n);
        
            for (size_t i = 0; i < n; ++i) {
                const std::string& full = sci_names[i];
                std::string partial = !species[i].empty() ? genus[i] + " " + species[i] : genus[i];
                if (!infra[i].empty()) partial += " " + infra[i];
                addRecord(full, partial, taxon_ids[i], i);
            }
        }

        void addRecord(const std::string& full_name,
               const std::string& partial_name, const std::string& taxon_id,
               size_t record_index)
        {
            sci_full_to_idx[full_name].push_back(record_index);
            sci_partial_to_idx[partial_name].push_back(record_index);
            taxon_id_to_idx.emplace(taxon_id, record_index);

            full_names.push_back(full_name);
            partial_names.push_back(partial_name);
        }

        std::vector<size_t> find_by_full_name(const std::string& name) {
            auto it = sci_full_to_idx.find(name);
            if (it != sci_full_to_idx.end()) {
                return it->second; // return all matches (all indices where name occurs in COL data)
            }
            return {}; // empty vector if not found
        }

        std::vector<size_t> find_by_partial_name(const std::string& name){
            auto it = sci_partial_to_idx.find(name);
            if (it != sci_partial_to_idx.end()) {
                return it->second; // return all matches (all indices where name occurs in COL data)
            }
            return {}; // empty vector if not found
        }


};



//======================================================================
// COL Record Class
//======================================================================
struct COLRecord{
    std::string taxonID;
    std::string parentNameUsageID;
    std::string acceptedNameUsageID;
    std::string scientificNameID;
    std::string taxonomicStatus;
    std::string taxonRank;
    std::string scientificName;
    std::string scientificNameAuthorship;
    std::string genericName;
    std::string specificEpithet;
    std::string infraspecificEpithet;

    std::string species;
    std::string genus;
    std::string family;
    std::string order;
    std::string class_;
    std::string phylum;
    std::string kingdom;

    COLRecord(ColData& col_data, long index){
        // Defensive: check keys exist before accessing
        auto get_val = [&](const std::string& key) -> std::string {
            auto it = col_data.find(key);
            if (it != col_data.end() && index < static_cast<long>(it->second.size())) {
                return it->second[index];
            }
            return "";
        };
    
        taxonID = get_val("dwc:taxonID");
        parentNameUsageID = get_val("dwc:parentNameUsageID");
        acceptedNameUsageID = get_val("dwc:acceptedNameUsageID");
        scientificNameID = get_val("dwc:scientificNameID");
        taxonomicStatus = get_val("dwc:taxonomicStatus");
        taxonRank = get_val("dwc:taxonRank");
        scientificName = get_val("dwc:scientificName");
        scientificNameAuthorship = get_val("dwc:scientificNameAuthorship");
        genericName = get_val("dwc:genericName");
        specificEpithet = get_val("dwc:specificEpithet");
        infraspecificEpithet = get_val("dwc:infraspecificEpithet");

        species = get_val("species");
        genus = get_val("genus");
        family = get_val("family");
        order = get_val("order");
        class_ = get_val("class");
        phylum = get_val("phylum");
        kingdom = get_val("kingdom");
    }
};


//======================================================================
// Date Cleaning Result Classes
//======================================================================
struct TaxonResult {
    std::string taxon_raw;
    std::string authority_raw;
    std::string col_sci_name_match; // name of the column in col_data that matched the taxon
    std::string kingdom_pres;
    std::string phylum_pres;
    std::string class_pres;
    std::string order_pres;
    std::string family_pres;
    std::string genus_pres;
    std::string specific_epithet_pres;
    std::string species_pres;
    std::string authority_pres;
    std::string taxon_rank;
    std::string match_type;
    std::string match_score;
    std::string match_error;

    ColData& col_data;
    COLIndex& col_idx;

    TaxonResult(const std::string& input_taxon, const std::string& input_authority, ColData& col_data, COLIndex& col_idx) : 
    taxon_raw(input_taxon), 
    authority_raw(input_authority),
    col_sci_name_match(""),
    kingdom_pres(""), 
    phylum_pres(""),
    class_pres(""), 
    order_pres(""), 
    family_pres(""),
    genus_pres(""),
    specific_epithet_pres(""),
    species_pres(""),
    authority_pres(""),
    taxon_rank(""),
    match_type(""), 
    match_score(""),
    match_error(""),
    col_data(col_data),
    col_idx(col_idx)
    {}


    void clean_taxonomy(){
        if (taxon_raw.empty()){ // is input taxon blank?
            return;
        }
        if (boost::regex_match(taxon_raw, species_hypothesis_regex)){ // is input taxon a species hypothesis? (e.g., SH1125882.09FU)
            return;
        }
        COLExactMatch exact_match = get_exact_match();
        if (!exact_match.accepted_taxon_id.empty()){ // exact match found with accepted taxon id; do not proceed to fuzzy matching 
            match_type = exact_match.match_type;
            match_score = "100";
            col_sci_name_match = exact_match.sci_name;
            match_error = exact_match.error;
            get_tax_hier(exact_match.accepted_taxon_id);
        } else if (!exact_match.error.empty()){ // error detected during exact match; do not proceed to fuzzy match
            if (!exact_match.sci_name.empty()){
                match_type = exact_match.match_type;
                match_score = "100";
                col_sci_name_match = exact_match.sci_name;
            }
            match_error = exact_match.error;
        } else { // if no accpted taxon id found and there are no errors, proceed to fuzzy matching
            COLFuzzyMatch fuzzy_match = get_best_fuzzy_match(); // TODO integrate error reporting like with exact match
            if (!fuzzy_match.accepted_taxon_id.empty()){
                match_type = fuzzy_match.match_type;
                match_score = std::to_string(fuzzy_match.match_score);
                col_sci_name_match = fuzzy_match.sci_name;
                get_tax_hier(fuzzy_match.accepted_taxon_id);
            }
        }
        return;
    };


    COLExactMatch get_exact_match(){
        DEBUG_PRINT("Searching for exact match...");
        std::string match_type;

        //------------------------------------------------------------------------------------
        // Full name matching  (genus + species + authority)
        //------------------------------------------------------------------------------------
        // search for input taxon name first (may already include authority)
        std::string query_taxon_name = taxon_raw; 
        DEBUG_PRINT("Query: " + query_taxon_name);
        auto match_ind = col_idx.find_by_full_name(query_taxon_name);


        // search for input taxon name + authority (taxon name may not have already included authority)
        if (match_ind.size()==0 && authority_raw != ""){
            query_taxon_name = taxon_raw + " " + authority_raw;
            DEBUG_PRINT("Query: " + query_taxon_name);
            match_ind = col_idx.find_by_full_name(query_taxon_name);
        }

        if (match_ind.size()>0){
            match_type = "EXACT-FULL";
        }
        //------------------------------------------------------------------------------------
        // Partial name match (genus + species) - NO AUTHORITY
        //------------------------------------------------------------------------------------
        if (match_ind.size()==0){ // no full name matches (taxon + authority)
            query_taxon_name = taxon_raw; // without input authority appended
            DEBUG_PRINT("Query: " + query_taxon_name);
            match_ind = col_idx.find_by_partial_name(query_taxon_name);
            if (match_ind.size()>0){
                match_type = "EXACT-PARTIAL";
            }
        }

        //------------------------------------------------------------------------------------
        // Parse exact matches and return accepted TaxonID
        //------------------------------------------------------------------------------------
        std::string tax_status;
        if (match_ind.size()==1){ // one exact match
            DEBUG_PRINT("Found one exact match: " + col_data["dwc:scientificName"][match_ind[0]]);
            tax_status = col_data["dwc:taxonomicStatus"][match_ind[0]];
            DEBUG_PRINT("Tax status: " + tax_status);
            if (tax_status=="accepted"||tax_status=="provisionally accepted"){ // match has accepted status, return ID
                return {col_data["dwc:taxonID"][match_ind[0]], match_type, col_data["dwc:scientificName"][match_ind[0]]};
            } else if (!col_data["dwc:acceptedNameUsageID"][match_ind[0]].empty()){ // match has synonym status, return accepted taxon ID
                return {col_data["dwc:acceptedNameUsageID"][match_ind[0]], match_type, col_data["dwc:scientificName"][match_ind[0]]};
            } else { // match has doubtful status, no valid ID to return
                return {"", match_type, col_data["dwc:scientificName"][match_ind[0]], "exact match found, but has no accepted name"};
            }
        } else if (match_ind.size()>1) { // multiple matches
            DEBUG_PRINT("Found " + std::to_string(match_ind.size()) + " exact matches.");
            for (int i = 0; i < static_cast<int>(match_ind.size()); i++){ // check if any have accepted status
                tax_status = col_data["dwc:taxonomicStatus"][match_ind[i]];
                if (tax_status=="accepted"||tax_status=="provisionally accepted"){ // TODO what if they are multiple matches with accepted status?
                    return {col_data["dwc:taxonID"][match_ind[i]], match_type, col_data["dwc:scientificName"][match_ind[i]]}; // return accepted taxon ID and COL scientific name match
                }
            }
            std::unordered_map<std::string, std::pair<COLMatchType, COLScientificNameMatch>> accepted_taxon_ids;
            for (int i = 0; i < static_cast<int>(match_ind.size()); i++){ // if none have accepted status, then see if all are synonyms of the same accepted taxon
                tax_status = col_data["dwc:taxonomicStatus"][match_ind[i]];
                DEBUG_PRINT("Match " + std::to_string(i) + ": " + col_data["dwc:scientificName"][match_ind[i]] + " with status: " + col_data["dwc:taxonomicStatus"][match_ind[i]]);
                if (!col_data["dwc:acceptedNameUsageID"][match_ind[i]].empty()){
                    accepted_taxon_ids.emplace(col_data["dwc:acceptedNameUsageID"][match_ind[i]], std::make_pair(match_type, col_data["dwc:scientificName"][match_ind[i]]));
                }
            }
            if (accepted_taxon_ids.size()==1){ // all synonym matches had same accepted taxon ID
                return {accepted_taxon_ids.begin()->first, accepted_taxon_ids.begin()->second.first, accepted_taxon_ids.begin()->second.second}; // return accepted taxon ID, match_type and COL scientific name match
            } else { // either all matches have doubtful status or synonym matches had different accepted taxon IDs
                return {"", "", "", "multiple exact matches with different accepted names;"}; // no valid match found
            }
        } else{ // zero matches
            return {"", "", "", ""}; // no valid match found
        }
    }


    COLFuzzyMatch get_best_fuzzy_match() { // TODO integrate error reporting like with exact match
        DEBUG_PRINT("Searching for fuzzy match...");

        double best_score = -1.0;
        std::string best_match_accepted_id;
        std::string best_match_type;
        std::string best_match_name;

        std::string tax_status;

        for (size_t i = 0; i < col_idx.full_names.size(); ++i) {
            const auto& full   = col_idx.full_names[i];
            const auto& partial = col_idx.partial_names[i];

            double score_full   = rapidfuzz::fuzz::ratio(taxon_raw, full);
            double score_partial = rapidfuzz::fuzz::ratio(taxon_raw, partial);

            double score;
            std::string match_type;

            if (score_full >= score_partial) {
                score = score_full;
                match_type = "FUZZY-FULL";
            } else {
                score = score_partial;
                match_type = "FUZZY-PARTIAL";
            }

            if (score > best_score) {
                best_score = score;
                tax_status = col_data.at("dwc:taxonomicStatus")[i];
                best_match_accepted_id = tax_status == "accepted" || tax_status == "provisionally accepted"
                            ? col_data.at("dwc:taxonID")[i]
                            : col_data.at("dwc:acceptedNameUsageID")[i]; // TODO error handling for when acceptedNameUsage is empty
                best_match_type = match_type;
                best_match_name = full;
                // DEBUG_PRINT("Best match so far: " + best_match_accepted_id + "(" + col_data["dwc:scientificName"][i] + ") with score: " + std::to_string(best_score));
            }
        }
        DEBUG_PRINT("Best match: " + best_match_accepted_id + " with score: " + std::to_string(best_score) + " with match type: " + best_match_type);
        COLFuzzyMatch out {best_match_accepted_id, best_score, best_match_type, best_match_name};
        return out;
    };


    void get_tax_hier(COLAcceptedTaxonID col_taxon_id){
        DEBUG_PRINT("Getting tax hieracrhy...");

        COLRecord col_record = get_record_by_taxon_id(col_idx, col_taxon_id);
        
        specific_epithet_pres = col_record.specificEpithet;
        species_pres = col_record.species;
        authority_pres = col_record.scientificNameAuthorship;
        genus_pres = col_record.genus;
        family_pres = col_record.family;
        order_pres = col_record.order;
        class_pres = col_record.class_;
        phylum_pres = col_record.phylum;
        kingdom_pres = col_record.kingdom;
        taxon_rank = col_record.taxonRank;
    };

    COLRecord get_record_by_taxon_id(const COLIndex& idx,
                                    const std::string& col_taxon_id)
    {
        auto it = idx.taxon_id_to_idx.find(col_taxon_id);
        if (it == idx.taxon_id_to_idx.end()) {
            throw std::runtime_error("Failed to find COL taxon ID: " + col_taxon_id);
        }
        return COLRecord(const_cast<ColData&>(col_data), it->second);
    }
};






class TaxonResults { // TODO add match error
    public:
        std::vector<std::pair<std::string, std::string>> taxon_raw_u; // unique taxon name + authority
        std::vector<std::string> taxon_raw; // not unique
        std::vector<std::string> authority_raw; // not unique
        std::vector<std::string> col_sci_name_match;
        std::vector<std::string> kingdom_pres;
        std::vector<std::string> phylum_pres;
        std::vector<std::string> class_pres;
        std::vector<std::string> order_pres;
        std::vector<std::string> family_pres;
        std::vector<std::string> genus_pres;
        std::vector<std::string> specific_epithet_pres;
        std::vector<std::string> species_pres;
        std::vector<std::string> authority_pres;
        std::vector<std::string> taxon_rank;
        std::vector<std::string> match_type;
        std::vector<std::string> match_score;
        
        ColData& col_data;
        COLIndex& col_idx;

        // Map from unique date string to all indices in date_raw where it occurs
        std::unordered_map<std::string, std::vector<std::size_t>> taxon_raw_indices;

        TaxonResults(std::vector<std::string>& input_taxon_names, std::vector<std::string>& input_authority, ColData& col_data, COLIndex& col_idx)
            : taxon_raw_u(), 
            taxon_raw(input_taxon_names),
            authority_raw(input_authority),
            col_sci_name_match(input_taxon_names.size()),
            kingdom_pres(input_taxon_names.size()), 
            phylum_pres(input_taxon_names.size()),
            class_pres(input_taxon_names.size()),
            order_pres(input_taxon_names.size()),
            family_pres(input_taxon_names.size()),
            genus_pres(input_taxon_names.size()),
            specific_epithet_pres(input_taxon_names.size()),
            species_pres(input_taxon_names.size()),
            authority_pres(input_taxon_names.size()),
            taxon_rank(input_taxon_names.size()),
            match_type(input_taxon_names.size()),
            match_score(input_taxon_names.size()),
            col_data(col_data),
            col_idx(col_idx)
            {
                // make list of unique taxon names for faster processing
                std::unordered_set<std::string> seen;
                for (long unsigned i = 0; i < static_cast<long unsigned>(input_taxon_names.size()); ++i) {
                    std::string concat_name = input_authority[i] != "" ? input_taxon_names[i] + " " + input_authority[i] : input_taxon_names[i];
                    if (seen.emplace(concat_name).second) {
                        taxon_raw_u.push_back(std::make_pair(input_taxon_names[i], input_authority[i]));
                    }
                    taxon_raw_indices[concat_name].push_back(i);
                }
            }

        // add_result: assign to all occurrences of the taxon name in taxon_raw vector
        int add_result(TaxonResult& taxon_result){
            std::string concat_name = taxon_result.authority_raw != "" ? taxon_result.taxon_raw + " " + taxon_result.authority_raw : taxon_result.taxon_raw;
            auto it = taxon_raw_indices.find(concat_name);
            if (it == taxon_raw_indices.end()) return -1;

            for (std::size_t i : it->second) { // TODO add match error
                col_sci_name_match[i] = taxon_result.col_sci_name_match;
                kingdom_pres[i] = taxon_result.kingdom_pres;
                phylum_pres[i] = taxon_result.phylum_pres;
                class_pres[i] = taxon_result.class_pres;    
                order_pres[i] = taxon_result.order_pres;
                family_pres[i] = taxon_result.family_pres;
                genus_pres[i] = taxon_result.genus_pres;
                specific_epithet_pres[i] = taxon_result.specific_epithet_pres;
                species_pres[i] = taxon_result.species_pres;
                authority_pres[i] = taxon_result.authority_pres;
                taxon_rank[i] = taxon_result.taxon_rank;
                match_type[i] = taxon_result.match_type;
                match_score[i] = taxon_result.match_score;
            }
            return 0;
        }

        Rcpp::DataFrame to_data_frame(){ // TODO add match error
            unsigned long n = taxon_raw.size();
            Rcpp::CharacterVector r_taxon_raw_vec(n);
            Rcpp::CharacterVector r_authority_raw_vec(n);
            Rcpp::CharacterVector r_col_sci_name_match(n);
            Rcpp::CharacterVector r_kingdom_pres(n);
            Rcpp::CharacterVector r_phylum_pres(n);
            Rcpp::CharacterVector r_class_pres(n);
            Rcpp::CharacterVector r_order_pres(n);
            Rcpp::CharacterVector r_family_pres(n);
            Rcpp::CharacterVector r_genus_pres(n);
            Rcpp::CharacterVector r_specific_epithet_pres(n);
            Rcpp::CharacterVector r_species_pres(n);
            Rcpp::CharacterVector r_authority_pres(n);
            Rcpp::CharacterVector r_taxon_rank(n);
            Rcpp::CharacterVector r_match_type(n);
            Rcpp::CharacterVector r_match_score(n);

            for (unsigned long i = 0; i < n; ++i) { // TODO add match error
                r_taxon_raw_vec[i] = !taxon_raw[i].empty() ? Rcpp::String(taxon_raw[i]) : NA_STRING;
                r_authority_raw_vec[i] = !authority_raw[i].empty() ? Rcpp::String(authority_raw[i]) : NA_STRING;
                r_col_sci_name_match[i] = !col_sci_name_match[i].empty() ? Rcpp::String(col_sci_name_match[i]) : NA_STRING;
                r_kingdom_pres[i] = !kingdom_pres[i].empty() ? Rcpp::String(kingdom_pres[i]) : NA_STRING;
                r_phylum_pres[i] = !phylum_pres[i].empty() ? Rcpp::String(phylum_pres[i]) : NA_STRING;
                r_class_pres[i] = !class_pres[i].empty() ? Rcpp::String(class_pres[i]) : NA_STRING;
                r_order_pres[i] = !order_pres[i].empty() ? Rcpp::String(order_pres[i]) : NA_STRING;
                r_family_pres[i] = !family_pres[i].empty() ? Rcpp::String(family_pres[i]) : NA_STRING;
                r_genus_pres[i] = !genus_pres[i].empty() ? Rcpp::String(genus_pres[i]) : NA_STRING;
                r_specific_epithet_pres[i] = !specific_epithet_pres[i].empty() ? Rcpp::String(specific_epithet_pres[i]) : NA_STRING;
                r_species_pres[i] = !species_pres[i].empty() ? Rcpp::String(species_pres[i]) : NA_STRING;
                r_authority_pres[i] = !authority_pres[i].empty() ? Rcpp::String(authority_pres[i]) : NA_STRING;
                r_taxon_rank[i] = !taxon_rank[i].empty() ? Rcpp::String(taxon_rank[i]) : NA_STRING;
                r_match_type[i] = !match_type[i].empty() ? Rcpp::String(match_type[i]) : NA_STRING;
                r_match_score[i] = !match_score[i].empty() ? Rcpp::String(match_score[i]) : NA_STRING;
            }
            return Rcpp::DataFrame::create(
                Rcpp::Named("taxon_raw") = r_taxon_raw_vec,
                Rcpp::Named("authority_raw") = r_authority_raw_vec,
                Rcpp::Named("sci_name_match") = r_col_sci_name_match,
                Rcpp::Named("kingdom_pres") = r_kingdom_pres,
                Rcpp::Named("phylum_pres") = r_phylum_pres,
                Rcpp::Named("class_pres") = r_class_pres,
                Rcpp::Named("order_pres") = r_order_pres,
                Rcpp::Named("family_pres") = r_family_pres,
                Rcpp::Named("genus_pres") = r_genus_pres,
                Rcpp::Named("specific_epithet_pres") = r_specific_epithet_pres,
                Rcpp::Named("species_pres") = r_species_pres,
                Rcpp::Named("authority_pres") = r_authority_pres,
                Rcpp::Named("taxon_rank") = r_taxon_rank,
                Rcpp::Named("match_type") = r_match_type,
                Rcpp::Named("match_score") = r_match_score
            );
        };

        int clean_taxonomy(){
            unsigned long n = taxon_raw_u.size();
            for (unsigned long i = 0; i < n; ++i) {
                DEBUG_PRINT("Cleaning taxon " + std::to_string(i+1) + " of " + std::to_string(n) + ": " + taxon_raw_u[i].first + " ...");
                TaxonResult taxon_result(taxon_raw_u[i].first, taxon_raw_u[i].second, col_data, col_idx);
                taxon_result.clean_taxonomy();
                add_result(taxon_result);
                DEBUG_PRINT(" Done.");

                // progress
                unsigned long print_freq = taxon_raw_u.size()*0.01 < 50 ? 50 : taxon_raw_u.size()*0.01;
                if ((i+1)%print_freq==0 || (i+1)==n){
                    DEBUG_PRINT(i%print_freq);
                    print_progress_bar(i+1,n);
                    DEBUG_PRINT(i%print_freq);
                }
            }
            return 0;
        };
};




//======================================================================
// Clean taxonomy function
//======================================================================
// [[Rcpp::depends(BH)]]
// [[Rcpp::export]]
Rcpp::DataFrame clean_taxonomy_cpp(const Rcpp::CharacterVector& input_taxon_names, const Rcpp::CharacterVector& input_authority, const Rcpp::DataFrame& input_col_data) {
    try{
        DEBUG_PRINT("Cleaning taxonomy...");
        std::vector<std::string> input_taxon_names_cpp = convert_r_vec_to_cpp_vec(input_taxon_names);
        std::vector<std::string> input_authority_cpp = convert_r_vec_to_cpp_vec(input_authority);
        ColData col_data = convert_r_df_to_cpp_map(input_col_data);
        COLIndex col_idx(col_data);
        TaxonResults taxon_results(input_taxon_names_cpp, input_authority_cpp, col_data, col_idx);
        taxon_results.clean_taxonomy();
        DEBUG_PRINT("Taxonomy cleaned.");
        return taxon_results.to_data_frame();
    } catch (std::exception &ex) {
        Rcpp::stop("C++ exception: %s", ex.what());
    } catch (...) {
        Rcpp::stop("Unknown C++ exception");
    }
};