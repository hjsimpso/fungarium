#' @title Calculate abundance
#'
#' @description
#' Calculate abundance. ...
#'
#' @param data  `Data.frame`.
#' @param of    Character. ...
#' @param per Character. ...
#' @param transformation Character. ...
#' @param pseudocount Numeric. ...
#'
#' @return Input `data.frame` with the following fields appended:
#' \describe{
#' \item{\code{abund}}{Abundance of each value in the `of` field per each unique combination of the `per` fields.}
#' \item{\code{total_occ}}{Total number of occrrences in the field specified by `per`.}
#' \item{\code{rel_abund}}{Relative abundance of each value in the `of` field per each unique combination of the `per` fields.}
#' }
#' @export
#' @examples
#' TODO

calc_abundance <- function(data,
                           of="species_pres",
                           per=c("year_parsed", "grid_cell_id"),
                           transformation=NULL,
                           pseudocount=0){
  # check args
  checkmate::assert_data_frame(data)
  checkmate::assert_character(of, max.len = 1)
  checkmate::assert_character(per)
  checkmate::assert_character(transformation, max.len = 1, null.ok = TRUE)
  checkmate::assert_numeric(pseudocount)

  checkmate::assert_choice(of, colnames(data))
  lapply(per, checkmate::assert_choice, choices=colnames(data), .var.name='per')
  checkmate::assert_choice(transformation, c("rel_abund", "clr"), null.ok = TRUE)

  # get abund
  data.table::setDT(data)
  .datatable.aware = TRUE
  data <- data[!is.na(data[[of]])] # remove NA values
  data <- data[, .(abund = .N), by = c(of, per)]

  # convert to wide format
  data <- make_wide(data, of, per, "abund", pseudocount = pseudocount, transformation=transformation)

  # return data
  data.table::setDF(data)
  class(data) <- c("fg_abund_table", class(data))
  return(data)
}

# reshape to wide
make_wide <- function(data, of, per, value.var, pseudocount, transformation){
  .datatable.aware = TRUE
  of_values <- unique(data[[of]])
  formula_wide <- as.formula(paste0(paste0(per, collapse = " + "), " ~ ", of))
  data <- data.table::dcast(data, formula_wide, value.var = value.var, fill = pseudocount)
  data <- data[, "total_occ" := rowSums(.SD), .SDcols = setdiff(names(data), per)]
  data.table::setcolorder(data, c(per, "total_occ", of_values))

  if (!is.null(transformation)){ # transform abundance
    if (transformation=="rel_abund"){
      data[, (of_values) := lapply(.SD, function(x) x / total_occ), .SDcols = of_values]
    } else{ # clr
      data_clr <- compositions::clr(data)
      data <- cbind(data_clr, data[,..per])
    }

  }
  return(data)
}

