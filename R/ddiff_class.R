#' obtain types of variables from given two datasets
#'
#' @description This function could obtain types of variables from given two datasets. Note that types of variables are defined as follows : continuous, categorical and binary
#' @param dat1 First Data need to be compared.
#' @param dat2 Second Data need to be compared.
#' @param key Unique identifiers to link two datasets
#' @param ... potential argument to added
#' @return The types of variables from given two datasets.
#' @examples
#' d = test_data()
#' ddiff_rpt <- ddiff_class(d$identical$new, d$identical$old, c("ID", "Days"))
#' data.tree::FromListSimple(ddiff_rpt)
#' str(ddiff_rpt)
#' @export
#'
ddiff_class <- function(dat1, dat2, key, ...){
  #' TODO: this is fragile as it relies on what classes are defined for columns
  #' Ex. try converting factor to character and nlevels logic breaks
  #' Consider looking into readr::guess_parser for a more robust implementation 
  #' of guess while providing room for guess verification or user specification
  #' of the classes 
  #' 
  #' TODO: needs assertion of input params. What happens if key doesn't exist
  #' 
  #' TODO: In addition to the base classes, we may want to enable special case
  #' for time series given its prevalence 
  dat1$id <- tidyr::unite(as.data.frame(dat1[,key]),  key, remove = FALSE, na.rm = FALSE)$key
  dat2$id <- tidyr::unite(as.data.frame(dat2[,key]),  key, remove = FALSE, na.rm = FALSE)$key
  n1 = nrow(dat1); d1 = ncol(dat1); n2 = nrow(dat2); d2 = ncol(dat2)
  col_names1 = names(dat1)
  con_pos1 = sapply(dat1, class) %in% c("numeric", "integer")
  cat_pos1 = sapply(dat1, nlevels) >= 3
  bin_pos1 = sapply(dat1, nlevels) == 2
  id_pos1 = col_names1 == "id"

  col_names2 = names(dat2)
  con_pos2 = sapply(dat1, class) %in% c("numeric", "integer")
  cat_pos2 = sapply(dat2, nlevels) >= 3
  bin_pos2 = sapply(dat2, nlevels) == 2
  id_pos2 = col_names2 == "id"

  diff_objects <- list()
  data_1 = list(continous = list(col_names1[con_pos1]),
                categorical = list(col_names1[cat_pos1]),
                binary = list(col_names1[bin_pos1]))
  data_2 = list(continous = list(col_names2[con_pos2]),
                categorical = list(col_names2[cat_pos2]),
                binary = list(col_names2[bin_pos2]))

  diff_objects <- list(key = list(key), data_1 = data_1 , data_2 = data_2)
  return(diff_objects)
}
