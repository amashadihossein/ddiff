#' obtain and compare types of variables from given two datasets
#'
#' @description obtain and compare types of variables from given two datasets. The comparison are made on three main aspects : continuous, categorical and binary
#' @param dat1 First Data need to be compared.
#' @param dat2 Second Data need to be compared.
#' @param key Unique identifiers to link two datasets
#' @param ... potential argument to added
#' @return The types of variables from given two datasets.
#' @examples
#' n = 100
#' d = generate_test_data(n)
#' result <- get_variable_class(d$identical$new, d$identical$old, "id")
#' result$result_con
#' result$result_cat
#' result$result_bin
#' @export
get_variable_class <- function(dat1, dat2, key, ...){
  n1 = nrow(dat1); d1 = ncol(dat1); n2 = nrow(dat2); d2 = ncol(dat2)
  col_names1 = names(dat1)
  con_pos1 = sapply(dat1, class) == "numeric"
  cat_pos1 = sapply(dat1, nlevels) >= 3
  bin_pos1 = sapply(dat1, nlevels) == 2
  id_pos1 = col_names1 == key

  col_names2 = names(dat2)
  con_pos2 = sapply(dat2, class) == "numeric"
  cat_pos2 = sapply(dat2, nlevels) >= 3
  bin_pos2 = sapply(dat2, nlevels) == 2
  id_pos2 = col_names2 == key

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
