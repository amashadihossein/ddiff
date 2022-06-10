ddiff3 <- function(dat1, dat2, key, ...){
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

summary.con <- function(dat1, dat2, objective, fun...){
  #objective = ddiff_rpt
  data_1_summary <- rbind(sapply(dat1[, unlist(objective$data_1$continous)], summary)
                          , sapply(dat1[, unlist(objective$data_1$continous)], function(x) sqrt(var(x) / length(x))))
  data_2_summary <- rbind(sapply(dat2[, unlist(objective$data_2$continous)], summary)
                          , sapply(dat2[, unlist(objective$data_1$continous)], function(x) sqrt(var(x) / length(x))))
  nrow(data_1_summary)
  #colnames_1 <- colnames(data_2_summary)
  #data.table::chmatch(colnames(data_1_summary), colnames_1[c(1,3,2)])
  pos_data2 <- data.table::chmatch(colnames(data_1_summary), colnames(data_2_summary))
  summary_table <- matrix(0, nrow = nrow(data_1_summary), ncol = ncol(data_1_summary) + ncol(data_2_summary))
  summary_table[, seq(1, 2*ncol(data_1_summary) - 1, 2)] <- data_1_summary
  pos_summary2 <-  seq(2, 2*ncol(data_2_summary), 2)
  summary_table[, pos_summary2[pos_data2]] <- data_2_summary
  row_name <- row.names(data_1_summary)
  row_name[length(row_name)] <- "standard error"
  row.names(summary_table) <- row_name
  col_name <- character(ncol(data_1_summary) + ncol(data_2_summary))
  col_name[seq(1, 2*ncol(data_1_summary) - 1, 2)] <- paste(colnames(data_1_summary), "d1", sep=".")
  col_name[pos_summary2[pos_data2]] <- paste(colnames(data_2_summary), "d2", sep=".")
  colnames(summary_table) <- col_name
  return(summary_table)
}

summary.cat <- function(dat1, dat2, objective, fun...){
  cat_table1 <- data.frame(table(dat1[, unlist(objective$data_1$categorical)]))
  cat_table2 <- data.frame(table(dat2[, unlist(objective$data_2$categorical)]))
  summary_table_cat <- cbind(cat_table1, cat_table2)
  colnames(summary_table_cat)[c(1,3)] = c(paste(unlist(objective$data_1$categorical), "d1", sep="."), paste(unlist(objective$data_2$categorical), "d2", sep="."))
  return(summary_table_cat)
}

summary.bin <- function(dat1, dat2, objective, fun...){
  cat_table1 <- data.frame(table(dat1[, unlist(objective$data_1$binary)]))
  cat_table2 <- data.frame(table(dat2[, unlist(objective$data_2$binary)]))
  summary_table_cat <- cbind(cat_table1, cat_table2)
  colnames(summary_table_cat)[c(1,3)] = c(paste(unlist(objective$data_1$binary), "d1", sep="."), paste(unlist(objective$data_2$binary), "d2", sep="."))
  return(summary_table_cat)
}
