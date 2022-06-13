get_variable_stat <- function(dat1, dat2, key, 
      measure_arg_con = c("min", "max", "median", "mean", "standard error"), 
      measure_arg_cat = c("table"), measure_arg_bin = c("table"), ...){
  abc <- function(...){
    getwd()
    source('experiment/ddiff.R')
    source('experiment/diff_info_1.R')
    source('experiment/d_test.R')
    source('univariate diff/get_variable_class.R')
    d <- d_test()
    dat1 = d$records_added$new
    dat2 = d$records_added$old
    key = "id"
    n1 = nrow(dat1); d1 = ncol(dat1); n2 = nrow(dat2); d2 = ncol(dat2)
    measure_arg_con = c("min", "max", "median", "mean")
    measure_arg_cat = c("table")
    objective = get_variable_class(dat1, dat2, "id")
  }  
  objective = get_variable_class(dat1, dat2,  key)
  diff_result <- NULL
  
  if("standard error" %in% measure_arg_con){
    con_summary1 <- data.frame(sapply(dat1[, unlist(objective$data_1$continous)], function(x) sqrt(var(x) / length(x))))
    con_summary2 <- data.frame(sapply(dat2[, unlist(objective$data_2$continous)], function(x) sqrt(var(x) / length(x))))
    abc1 <- merge(con_summary1, con_summary2, by="row.names")
    abc2 <- cbind(rep("standard error", ncol(abc1)), abc1)
    colnames(abc2) <- c("stat", "variable", "data1", "data2")
    measure_arg_con = measure_arg_con[!measure_arg_con == "standard error"]
  }
  
  for (arg in measure_arg_con) {
    #arg = "min"
    test1 = data.frame(sapply(dat1[, unlist(objective$data_1$continous)], arg))
    test2 = data.frame(sapply(dat2[, unlist(objective$data_2$continous)], arg))
    abc <- merge(test1, test2, by="row.names")
    diff_result <- rbind(diff_result, cbind(rep(arg, ncol(abc)),  abc))
  }
  colnames(diff_result) <- c("stat", "variable", "data1", "data2")
  
  diff_result <- rbind(diff_result, abc2)
  
  diff_result_cat <- NULL
  for (arg in measure_arg_cat) {
    #arg = "table"
    cat_table1 <- data.frame(apply(dat1[, unlist(objective$data_1$categorical), drop=F], 2, arg))
    cat_table2 <- data.frame(apply(dat2[, unlist(objective$data_2$categorical), drop=F], 2, arg))
    abc <- merge(cat_table1, cat_table2, by="row.names")
    diff_result_cat <- rbind(diff_result_cat, abc)
  }
  
  diff_result_bin <- NULL
  for (arg in measure_arg_bin) {
    #arg = "table"
    bin_table1 <- data.frame(apply(dat1[, unlist(objective$data_1$binary), drop=F], 2, arg))
    bin_table2 <- data.frame(apply(dat2[, unlist(objective$data_2$binary), drop=F], 2, arg))
    abc <- merge(bin_table1, bin_table2, by="row.names")
    diff_result_bin <- rbind(diff_result_bin, abc)
  }
   return(list(result_con = diff_result, result_cat = diff_result_cat, result_bin = diff_result_bin))
  }

