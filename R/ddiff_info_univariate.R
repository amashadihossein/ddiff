#' @description obtain and compare statistical information from given two datasets. The comparison are made on following aspects :
#' 1. whether the types of variables from given two datasets are the same
#' 2. whether column names from given two datasets are the same
#' 3. whether row names from given two datasets are the same
#' 4. whether attributes from given two datasets are the same
#' 5. whether the information given two datasets are the same (shuffled rows or columns)
#' 6. whether the given two datasets are equivalent in R memory
#' @param dat1 First Data need to be compared.
#' @param dat2 Second Data need to be compared.
#' @param key Unique identifiers to link two datasets
#' @param measure_arg_con list of function for summary statistics of continuous response. The defalut list is c("min", "max", "median", "mean", "standard error")
#' @param measure_arg_cat list of function for summary statistics of categorical response
#' @param measure_arg_bin list of function for summary statistics of binary response
#' @param dis_arg list of distance for measuring the difference of variables in each datasets
#' @param ... potential argument to added
#' @return TBC
#'
#' @examples
#'
#' @export
#'
ddiff_info_univariate <- function(dat1, dat2, key, measure_arg_con = list(cov = cov),
                                  measure_arg_cat = list(table = table), measure_arg_bin = list(table = table), dis_arg = list("euclidean", "manhattan", "gower"), ...){
  objective = get_variable_class(dat1, dat2,  key)
  diff_result <- NULL
  abc2 <- NULL

  if("standard error" %in% measure_arg_con){
    con_summary1 <- data.frame(sapply(dat1[, unlist(objective$data_1$continous)], function(x) sqrt(var(x) / length(x))))
    con_summary2 <- data.frame(sapply(dat2[, unlist(objective$data_2$continous)], function(x) sqrt(var(x) / length(x))))
    std_result <- merge(con_summary1, con_summary2, by="row.names")
    std_result_nam <- cbind(rep("standard error", nrow(std_result)), std_result)
    colnames(std_result_nam) <- c("stat", "variable", "data1", "data2")
    measure_arg_con = measure_arg_con[!measure_arg_con == "standard error"]
  }

  for (arg in names(measure_arg_con)) {
    test1 = data.frame(sapply(dat1[, unlist(objective$data_1$continous)], measure_arg_con[[arg]]))
    test2 = data.frame(sapply(dat2[, unlist(objective$data_2$continous)], measure_arg_con[[arg]]))
    abc <- merge(test1, test2, by="row.names")
    diff_result <- rbind(diff_result, cbind(rep(arg, nrow(abc)),  abc))
  }
  colnames(diff_result) <- c("stat", "variable", "data1", "data2")
  diff_result <- rbind(diff_result, abc2)

  diff_result_cat <- list()
  for (arg in names(measure_arg_cat)) {
    cat_table1 <- data.frame(apply(dat1[, unlist(objective$data_1$categorical), drop=F], 2, measure_arg_cat[[arg]]))
    cat_table2 <- data.frame(apply(dat2[, unlist(objective$data_2$categorical), drop=F], 2, measure_arg_cat[[arg]]))
  }

  for (i in 1:length(unlist(objective$data_1$categorical))){
    names = unlist(objective$data_1$categorical)
    result_cat <- merge(cat_table1[ , i], cat_table2[ , i], by="row.names")
    result_cat[,1] = names(table(dat1[, names[i], drop=F]))
    diff_result_cat <- append(diff_result_cat, list(result_cat))
    colnames(diff_result_cat[[i]]) <- c(names[i], "data1", "data2")
  }

  diff_result_bin <- list()
  for (arg in names(measure_arg_bin)) {
    bin_table1 <- data.frame(apply(dat1[, unlist(objective$data_1$binary), drop=F], 2, measure_arg_bin[[arg]]))
    bin_table2 <- data.frame(apply(dat2[, unlist(objective$data_2$binary), drop=F], 2, measure_arg_bin[[arg]]))
  }

  for (i in 1:length(unlist(objective$data_1$binary))){
    names = unlist(objective$data_1$binary)
    result_bin <- merge(bin_table1[ , i], bin_table2[ , i], by="row.names")
    result_bin[,1] = names(table(dat1[, names[i], drop=F]))
    diff_result_bin <- append(diff_result_bin, list(result_bin))
    colnames(diff_result_bin[[i]]) <- c(names[i], "data1", "data2")
  }

  return(list(result_con = diff_result, result_cat = diff_result_cat, result_bin = diff_result_bin))
}
