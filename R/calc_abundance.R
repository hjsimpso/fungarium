#' @title Calculate abundance
#'
#' @description
#' Calculate abundances for a particular variable (e.g., species) based on 
#' specified grouping variables (e.g., year, country).
#'
#' @param data  Data.frame.
#' @param of    Character. Name of column containing the variables for which you are calculating abundance. Default is "species_pres".
#' @param per Character. Vector of column names. Abundance for each "`of`" variable will be calculated for each combination of "`per`" variables. Default is c("year_parsed", "grid_cell_id").
#' @param relative Logical. Convert abundance to relative abundance.
#'
#' @return Data.frame containing abundance values for each "`of`" variable (as 
#' columns) and each combination of "`per`" variables as rows. Total occurrences 
#' for each row are recorded in "`total_occ`" column.
#' 
#' @export
#' @examples
#' library(fungarium)
#' data(fomitopsidaceae_fully_cleaned) #import sample data set
#' grid <- assign_grid(fomitopsidaceae_fully_cleaned)
#' grid$data <- grid$data[!is.na(grid$data$grid_cell_id)&!is.na(grid$data$year_parsed),]
#' abundance <- calc_abundance(grid$data)

calc_abundance <- function(data,
                           of="species_pres",
                           per=c("year_parsed", "grid_cell_id"),
                           relative=FALSE){
  # check args
  checkmate::assert_data_frame(data)
  checkmate::assert_character(of, max.len = 1)
  checkmate::assert_character(per)
  checkmate::assert_logical(relative, max.len = 1)

  checkmate::assert_choice(of, colnames(data))
  lapply(per, checkmate::assert_choice, choices=colnames(data), .var.name='per')

  # get abund
  data.table::setDT(data)
  data <- data[!is.na(data[[of]]),] # remove NA values
  data <- data[, .(abund = .N), by = c(of, per)]

  # convert to wide format
  data <- make_wide(data, of, per, "abund", relative=relative)

  # return data
  data.table::setDF(data)
  return(data)
}

# reshape to wide
make_wide <- function(data, of, per, value.var, relative){
  of_values <- unique(data[[of]])
  formula_wide <- as.formula(paste0(paste0(per, collapse = " + "), " ~ ", of))
  data <- data.table::dcast(data, formula_wide, value.var = value.var, fill = 0)
  data <- data[, "total_occ" := rowSums(.SD), .SDcols = setdiff(names(data), per)]
  data.table::setcolorder(data, c(per, "total_occ", of_values))

  # transform to relative abundance
  if (relative){
    data[, (of_values) := lapply(.SD, function(x) x / total_occ), .SDcols = of_values]
  }

  return(data)
}

# needed for proper subsetting of data.tables without importing whole package
.datatable.aware <- TRUE
