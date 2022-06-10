ddiff2 <- function(dat1, dat2, key, ...){
  dat1 = d$identical$new
  dat2 = d$identical$old
  key = "id"
  n1 = nrow(dat1); d1 = ncol(dat1); n2 = nrow(dat2); d2 = ncol(dat2)

  ddiff_obj <- list()
  ddiff_obj$identical <- ddiff_obj$info_match <- ddiff_obj$attributes_match <- ddiff_obj$row_match <- ddiff_obj$column_match <- ddiff_obj$class_match <- F

  ddiff_obj_info <- list()

  if(class(dat1) != "data.frame"  | class(dat2) != "data.frame"){
    return("error : input must be data frame objects")
  }

  #identical
  if(identical(x = dat1, y = dat2)){
    ddiff_obj$identical <- T
    ddiff_obj$message <- paste0(deparse(substitute(dat1)), " and ",  deparse(substitute(dat2)), " are identical")
    class1 <- sapply(dat1, class)
    class2 <- sapply(dat2, class)
    summary_con1 <- sapply(dat1[, class1 == "numeric"], summary)
    summary_con2 <- sapply(dat2[, class2 == "numeric"], summary)
    #lapply(seq_len(ncol(summary_con1)), function(i) summary_con1[,i])

    ddiff_obj_info$con <- list(con_dat1 = list(summary_con1) , con_dat2 = list(summary_con2))
    #ddiff_obj_info$con[1]
    #ddiff_obj_info$con[2]
    summary_bin1 <- sapply(dat1[, class1 == "character"], unique)
    summary_bin2 <- sapply(dat2[, class2 == "character"], unique)
    names1 <- names(summary_bin1)
    names2 <- names(summary_bin2)
    table(dat1[, names1[sapply(summary_bin1, length) == 2]])
    table(dat2[, names2[sapply(summary_bin2, length) == 2]])

    summary_bin1 <- sapply(dat1[, class1 == "character"], table)
    summary_bin2 <- sapply(dat2[, class2 == "character"], table)
    ddiff_obj_info$cat <- list(cat_dat1 = list(summary_bin1[sapply(summary_bin1, length) < n1]), cat_dat2 = list(summary_bin2[sapply(summary_bin2, length) < n2]))
  }
  return(ddiff_obj_info)
}

library(data.tree)

root <- Node$new("Univariate diff")
con <- root$AddChild("Continuous")
con_dat_1 <- con$AddChild("data 1")
con_dat_2 <- con$AddChild("data 2")
cat <- root$AddChild("Categorical")
cat_dat_1 <- cat$AddChild("data 1")
cat_dat_2 <- cat$AddChild("data 2")
bin <- root$AddChild("Binary")
bin_dat_1 <- bin$AddChild("data 1")
bin_dat_2 <- bin$AddChild("data 2")
print(root)

ddiff_objective <- as.list(root)
#ddiff_objective$Continuous$`data 1` <- summary_con1
data.tree::FromListSimple(ddiff_objective)

ddiff_objective$Continuous$`data 1` <- list(nm_1 = summary_con1[,1], nm_2 = summary_con1[,2])
data.tree::FromListSimple(ddiff_objective)

colnames(dat1)
sapply(dat1, class)
abc <- attributes(dat1)
class(abc)

abc <- as.matrix(sapply(dat1[, class1 == "character"], as.factor), )
class(abc)
nlevels(as.factor(dat1$cat_1))

col_names1 = names(dat1)
con_pos1 = sapply(dat1, class) == "numeric"
cat_pos1 = sapply(dat1, class) == "factor"
id_pos1 = sapply(dat1, class) == "character"

col_names2 = names(dat2)
con_pos2 = sapply(dat2, class) == "numeric"
cat_pos2 = sapply(dat2, class) == "factor"
id_pos2 = sapply(dat2, class) == "character"

diff_objects <- list()
data_1 = list(continous = list(col_names1[con_pos1]),
             categorical = list(col_names1[cat_pos1]),
             binary = list(col_names1[cat_pos1]))
data_2 = list(continous = list(col_names2[con_pos2]),
             categorical = list(col_names2[cat_pos2]),
             binary = list(col_names2[cat_pos2]))

diff_objects <- list(data_1 = data_1 , data_2 = data_2)
data.tree::FromListSimple(diff_objects)

sapply(dat1[, unlist(diff_objects$data_1$continous)], summary)
sapply(dat1[, unlist(diff_objects$data_1$continous)], function(x) sqrt(var(x) / length(x)))
sapply(dat2[, unlist(diff_objects$data_2$continous)], summary)
sapply(dat1[, unlist(diff_objects$data_1$continous)], function(x) sqrt(var(x) / length(x)))

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
