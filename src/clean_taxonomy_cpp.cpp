//======================================================================
// Imports
//======================================================================

#include <Rcpp.h>
#include <boost/regex.hpp>
#include <string>
#include <vector>
#include <rapidfuzz/fuzz.hpp>

//======================================================================
// Macros
//======================================================================
// #define DEBUG_PRINT(x) Rcpp::Rcout << x << std::endl
#define DEBUG_PRINT(x)

//======================================================================
// Types
//======================================================================
typedef std::map<std::string, std::vector<std::string>> ColData;
typedef std::string COLTaxonID;
//======================================================================
// Helpers
//======================================================================

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

ColData convert_r_df_to_cpp_map(Rcpp::DataFrame input) {
    ColData result;

    Rcpp::CharacterVector names = input.names();
    for (int i = 0; i < names.size(); ++i) {
        std::string col_name = Rcpp::as<std::string>(names[i]);
        Rcpp::CharacterVector col_data = input[col_name];
        std::vector<std::string> col_data_cpp = convert_r_vec_to_cpp_vec(col_data);
        result[col_name] = col_data_cpp;
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
// COL Record Class
//======================================================================
struct COLRecord{
    std::string taxonID;
    std::string parentNameUsageID;
    std::string acceptedNameUsageID;
    std::string originalNameUsageID;
    std::string scientificNameID;
    std::string datasetID;
    std::string taxonomicStatus;
    std::string taxonRank;
    std::string scientificName;
    std::string scientificNameAuthorship;
    std::string notho;
    std::string genericName;
    std::string infragenericEpithet;
    std::string specificEpithet;
    std::string infraspecificEpithet;
    std::string cultivarEpithet;
    std::string nameAccordingTo;
    std::string namePublishedIn;
    std::string nomenclaturalCode;
    std::string nomenclaturalStatus;
    std::string taxonRemarks;
    std::string references;

    // COLRecord(const std::string& taxon_id = "", const std::string& parent_name_usage_id = "", const std::string& accepted_name_usage_id = "", 
    //           const std::string& original_name_usage_id = "", const std::string& scientific_name_id = "", 
    //           const std::string& dataset_id = "", const std::string& taxonomic_status = "", 
    //           const std::string& taxon_rank = "", const std::string& scientific_name = "", 
    //           const std::string& scientific_name_authorship = "", const std::string& notho = "", 
    //           const std::string& generic_name = "", const std::string& infrageneric_epithet = "", 
    //           const std::string& specific_epithet = "", const std::string& infraspecific_epithet = "", 
    //           const std::string& cultivar_epithet = "", const std::string& name_according_to = "",
    //           const std::string& name_published_in = "", const std::string& nomenclatural_code = "",
    //           const std::string& nomenclatural_status = "", const std::string& taxon_remarks = "",
    //           const std::string& references = "") : 
    // taxonID(taxon_id),  
    // parentNameUsageID(parent_name_usage_id), 
    // acceptedNameUsageID(accepted_name_usage_id),
    // originalNameUsageID(original_name_usage_id),        
    // scientificNameID(scientific_name_id),
    // datasetID(dataset_id),
    // taxonomicStatus(taxonomic_status),
    // taxonRank(taxon_rank),
    // scientificName(scientific_name),
    // scientificNameAuthorship(scientific_name_authorship),
    // notho(notho),   
    // genericName(generic_name),
    // infragenericEpithet(infrageneric_epithet),  
    // specificEpithet(specific_epithet),
    // infraspecificEpithet(infraspecific_epithet),
    // cultivarEpithet(cultivar_epithet),
    // nameAccordingTo(name_according_to),
    // namePublishedIn(name_published_in),
    // nomenclaturalCode(nomenclatural_code),
    // nomenclaturalStatus(nomenclatural_status),
    // taxonRemarks(taxon_remarks),
    // references(references)
    // {};

    COLRecord(ColData& col_data, long index){
        taxonID = col_data["dwc:taxonID"][index];
        parentNameUsageID = col_data["dwc:parentNameUsageID"][index];
        acceptedNameUsageID = col_data["dwc:acceptedNameUsageID"][index];
        originalNameUsageID = col_data["dwc:originalNameUsageID"][index];
        scientificNameID = col_data["dwc:scientificNameID"][index];
        datasetID = col_data["dwc:datasetID"][index];
        taxonomicStatus = col_data["dwc:taxonomicStatus"][index];
        taxonRank = col_data["dwc:taxonRank"][index];
        scientificName = col_data["dwc:scientificName"][index];
        scientificNameAuthorship = col_data["dwc:scientificNameAuthorship"][index];
        notho = col_data["col:notho"][index];
        genericName = col_data["dwc:genericName"][index];
        infragenericEpithet = col_data["dwc:infragenericEpithet"][index];
        specificEpithet = col_data["dwc:specificEpithet"][index];
        infraspecificEpithet = col_data["dwc:infraspecificEpithet"][index];
        cultivarEpithet = col_data["dwc:cultivarEpithet"][index];
        nameAccordingTo = col_data["dwc:nameAccordingTo"][index];
        namePublishedIn = col_data["dwc:namePublishedIn"][index];
        nomenclaturalCode = col_data["dwc:nomenclaturalCode"][index];
        nomenclaturalStatus = col_data["dwc:nomenclaturalStatus"][index];
        taxonRemarks = col_data["dwc:taxonRemarks"][index];
        references = col_data["dcterms:references"][index];
    }
};


//======================================================================
// Date Cleaning Result Classes
//======================================================================
struct TaxonResult {
    std::string taxon_raw;
    std::string authority_raw;
    std::string kingdom_pres;
    std::string class_pres;
    std::string order_pres;
    std::string family_pres;
    std::string genus_pres;
    std::string specific_epithet_pres;
    std::string species_pres;
    std::string authority_pres;

    ColData& col_data;

    TaxonResult(const std::string& input_taxon, const std::string& input_authority, ColData& col_data) : 
    taxon_raw(input_taxon), 
    authority_raw(input_authority), 
    kingdom_pres(""), 
    class_pres(""), 
    order_pres(""), 
    family_pres(""),
    genus_pres(""),
    specific_epithet_pres(""),
    species_pres(""),
    authority_pres(""),
    col_data(col_data) {}


    void clean_taxonomy(){
        if (get_exact_match().empty()){
            get_best_fuzzy_match();
        }
        return;
    };


    COLTaxonID get_exact_match(){
        DEBUG_PRINT("Searching for exact match...");
        std::vector<COLRecord> col_name_matches;

        //------------------------------------------------------------------------------------
        // Full name matching  (genus + species + authority)
        //------------------------------------------------------------------------------------
        std::string col_taxon_name;
        for (long unsigned i = 0; i < static_cast<long unsigned>(col_data["dwc:scientificName"].size()); i++) {
            col_taxon_name = col_data["dwc:scientificName"][i];
            // in dwca file, input taxon name may or not include the authority, but authority may still be listed in scientificNameAuthorship
            // chek if taxon name (or taxon name with authority appended) matches the col namne (which already has authority appended)
            if (taxon_raw == col_taxon_name || (taxon_raw + " " + authority_raw) == col_taxon_name) { // exact match: taxon + authority (FULL NAME MATCH)
                col_name_matches.push_back(COLRecord(col_data, i));
            }
        }
        //------------------------------------------------------------------------------------
        // Partial name match (genus + species) - NO AUTHORITY
        //------------------------------------------------------------------------------------
        if (col_name_matches.size()==0){ // no full name matches (taxon + authority)
            std::string col_genus;
            std::string col_spec_epithet;
            std::string col_infraspec_epithet;
            for (long unsigned i = 0; i < static_cast<long unsigned>(col_data["dwc:scientificName"].size()); i++) {
                col_genus = col_data["dwc:genericName"][i];
                col_spec_epithet = col_data["dwc:specificEpithet"][i];
                col_infraspec_epithet = col_data["dwc:infraspecificEpithet"][i];
                col_taxon_name = col_spec_epithet !="" ? col_genus + " " + col_spec_epithet : col_genus; // concatonate genus and species epithet names
                col_taxon_name = col_infraspec_epithet !="" ? col_taxon_name + " " + col_infraspec_epithet : col_taxon_name; // concatonate genus, spec epithet, AND infrspecies epithet names
                if (taxon_raw == col_taxon_name) { // exact match: taxon name only, no authority (PARTIAL NAME MATCH)
                    col_name_matches.push_back(COLRecord(col_data, i));
                }
            }
        }
        //------------------------------------------------------------------------------------
        // Parse exact matches and return accepted TaxonID
        //------------------------------------------------------------------------------------
        if (col_name_matches.size()==1){ // one exact match
            std::string tax_status = col_name_matches[0].taxonomicStatus;
            if (tax_status=="accepted"){ // match has accepted status, return ID
                return col_name_matches[0].taxonID;
            } else if (tax_status=="synonym"){ // match has synonym status, return accepted taxon ID
                return col_name_matches[0].acceptedNameUsageID;
            } else { // match has doubtful status, no valid ID to return
                return "";
            }
        } else if (col_name_matches.size()>1) { // multiple matches
            for (int i = 0; i < static_cast<int>(col_name_matches.size()); i++){ // check if any have accepted status
                if (col_name_matches[i].taxonomicStatus=="accepted"){ // TODO what if they are multiple matches with accepted status?
                    return col_name_matches[i].taxonID;
                }
            }
            std::unordered_set<std::string> accepted_taxon_ids;
            for (int i = 0; i < static_cast<int>(col_name_matches.size()); i++){ // if none have accepted status, then see if all are synonyms of the same accepted taxon
                if (col_name_matches[i].taxonomicStatus=="synonym"){
                    accepted_taxon_ids.insert(col_name_matches[i].acceptedNameUsageID);
                }
            }
            if (accepted_taxon_ids.size()==1){ // all synonym matches had same accepted taxon ID
                return *accepted_taxon_ids.begin();
            } else { // either all matches have doubtful status or synonym matches had different accepted taxon IDs
                return "";
            }
        } else{ // zero matches
            return "";
        }
    }


    FuzzyMatch get_best_fuzzy_match() {
        DEBUG_PRINT("Searching for fuzzy match...");
        
        std::string best_match_id;
        double best_match_score = -1.0;
        std::string best_match_type;

        double local_match_score;
        std::string local_match_type;

        double score_full_name;
        double score_partial_name;
        std::string partial_name;
        std::string col_genus;
        std::string col_spec_epithet;
        std::string col_infraspec_epithet;


        for (long unsigned i = 0; i < static_cast<long unsigned>(col_data["dwc:scientificName"].size()); i++) {
            // Test full scientific names (taxon + authority)
            score_full_name = rapidfuzz::fuzz::ratio(taxon_raw, col_data["dwc:scientificName"][i]);
            
            // Test partial scientific names (no authority)
            col_genus = col_data["dwc:genericName"][i];
            col_spec_epithet = col_data["dwc:specificEpithet"][i];
            col_infraspec_epithet = col_data["dwc:infraspecificEpithet"][i];
            partial_name = col_spec_epithet !="" ? col_genus + " " + col_spec_epithet : col_genus; // concatonate genus and species epithet names
            partial_name = col_infraspec_epithet !="" ? partial_name + " " + col_infraspec_epithet : partial_name; // concatonate genus, spec epithet, AND infrspecies epithet names

            score_partial_name = rapidfuzz::fuzz::ratio(taxon_raw, partial_name);

            // pick best score
            if (score_full_name >= score_partial_name){
                local_match_score = score_full_name;
                local_match_type = "FUZZY-FULL"; // matched taxon + authority
            }else{
                local_match_score = score_partial_name;
                local_match_type = "FUZZY-PARTIAL"; // matched taxon (without authority)
            }

            if (local_match_score > best_match_score) { // TODO - Account for condition where scores are equal but taxon names arte different?
                best_match_score = local_match_score;
                best_match_id = col_data["dwc:taxonomicStatus"][i] == "accepted" ? col_data["dwc:taxonID"][i]: col_data["dwc:acceptedNameUsageID"][i]; // TODO - error handling for if taxon is 'doubtful' status
                best_match_type = local_match_type;
                DEBUG_PRINT("Best match so far: " + best_match_id + "(" + col_data["dwc:scientificName"][i] + ") with score: " + std::to_string(best_match_score));
            }
        }
        DEBUG_PRINT("Best match: " + best_match_id + " with score: " + std::to_string(best_match_score) + " with match type: " + best_match_type);
        std::tuple<std::string, double, std::string> out {best_match_id, best_match_score, best_match_type};
        return out;
    };


    void get_accepted_taxonomy(){

    };

    void get_tax_hier(){

    };

};





class TaxonResults {
    public:
        std::vector<std::string> taxon_raw_u; // unique
        std::vector<std::string> taxon_raw; // not unique
        std::vector<std::string> kingdom_pres;
        std::vector<std::string> class_pres;
        std::vector<std::string> order_pres;
        std::vector<std::string> family_pres;
        std::vector<std::string> genus_pres;
        std::vector<std::string> specific_epithet_pres;
        std::vector<std::string> species_pres;
        std::vector<std::string> authority_pres;

};




//======================================================================
// Clean taxonomy function
//======================================================================
// [[Rcpp::depends(BH)]]
// [[Rcpp::export]]
Rcpp::DataFrame clean_taxonomy_cpp(const Rcpp::CharacterVector& input_taxon_names, const Rcpp::CharacterVector& input_authority, Rcpp::DataFrame& col_data) {
    try{
        DEBUG_PRINT("Cleaning dates...");
        std::vector<std::string> input_dates_cpp = convert_r_vec_to_cpp_vec(input_taxon_names);
        std::vector<std::string> input_dates_cpp = convert_r_vec_to_cpp_vec(input_authority);
        // clean func here...

        DEBUG_PRINT("Dates cleaned.");
        return Rcpp::DataFrame(); // TODO
    } catch (std::exception &ex) {
        Rcpp::stop("C++ exception: %s", ex.what());
    } catch (...) {
        Rcpp::stop("Unknown C++ exception");
    }
};