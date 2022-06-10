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