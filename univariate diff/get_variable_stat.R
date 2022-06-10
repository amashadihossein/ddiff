get_variable_stat <- function(dat1, dat2, key, 
      measure_arg_con = c("min", "max", "median", "mean"), 
      measure_arg_cat = c("table"), ...){
  #dat1 = d$records_added$new
  #dat2 = d$records_added$old
  #key = "id"
  #n1 = nrow(dat1); d1 = ncol(dat1); n2 = nrow(dat2); d2 = ncol(dat2)
  #measure_arg_con = c("min", "max", "median", "mean")
  #measure_arg_cat = c("table")
  objective = get_variable_class(dat1, dat2, "id")
  
  for (arg in measure_arg_con) {
    arg = "min"
    test1 = data.frame(sapply(dat1[, unlist(objective$data_1$continous)], arg))
    test2 = data.frame(sapply(dat2[, unlist(objective$data_2$continous)], arg))
    #test1 = sapply(dat1[, unlist(objective$data_1$continous)], arg)
    #test2 = sapply(dat2[, unlist(objective$data_2$continous)], arg)
    
    ###
    row.names(test1)
    merge(test1, test2, by.x = "surname")
    ?merge
  }
  
  con_summary1 <- data.frame(rbind(sapply(dat1[, unlist(objective$data_1$continous)], min)
                          , sapply(dat1[, unlist(objective$data_1$continous)], function(x) sqrt(var(x) / length(x)))))
  con_summary2 <- data.frame(rbind(sapply(dat2[, unlist(objective$data_2$continous)], min)
                          , sapply(dat2[, unlist(objective$data_1$continous)], function(x) sqrt(var(x) / length(x)))))
  cat_table1 <- data.frame(table(dat1[, unlist(objective$data_1$categorical)]))
  cat_table2 <- data.frame(table(dat2[, unlist(objective$data_2$categorical)]))
  bin_table1 <- data.frame(table(dat1[, unlist(objective$data_1$binary)]))
  bin_table2 <- data.frame(table(dat2[, unlist(objective$data_2$binary)]))
  
  stat_summary <- matrix(NA, nrow = , ncol = 3)
  
  
  }

