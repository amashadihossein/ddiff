#' obtain and compare custom multivariate summary statistics from given two datasets
#'
#' @description obtain and compare custom multivariate summary statistics from given two datasets.
#' @param dat1 First Data need to be compared.
#' @param dat2 Second Data need to be compared.
#' @param key Unique identifiers to link two datasets
#' @param measure_arg_con list of function for summary statistics of continuous response. The defalut list is list(cov = cov)
#' @param measure_arg_cat list of function for summary statistics of categorical response
#' @param measure_arg_bin list of function for summary statistics of binary response
#' @param ... potential argument to added
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' n = 100
#' d = generate_test_data(n)
#' #result = get_variable_multi_stat(d$identical$new, d$identical$old, "id")
#' #result$result_con
#' #result$result_cat
#' #result$result_bin
#' @export
#'
get_variable_multi_stat <- function(dat1, dat2, key,
                          measure_arg_con = list(cov = cov),
                          measure_arg_cat = list(table = table), measure_arg_bin = list(table = table), ...){
  abc <- function(...){
    library(ddiff)
    d = generate_test_data(50)
    dat1 = d$records_added$new
    dat2 = d$records_added$old
    key = "id"
    measure_arg_con = list(cov = cov)
    n1 = nrow(dat1); d1 = ncol(dat1); n2 = nrow(dat2); d2 = ncol(dat2)
    objective = get_variable_class(dat1, dat2, "id")
    dat1_svd = svd(cov(dat1[, unlist(objective$data_1$continous)]))
    dat1_svd$u
    sqrt(dat1_svd$d)
    prcomp(dat1[, unlist(objective$data_1$continous)])
    #pilots.pca
    dat2_svd = svd(cov(dat2[, unlist(objective$data_2$continous)]))
  }
  objective = get_variable_class(dat1, dat2,  key)
  diff_result <- NULL
  diff_result_con <- NULL
  for (arg in names(measure_arg_con)) {
    #arg = "cov"
    con_varcovar1 <- measure_arg_con[[arg]](data.frame(dat1[, unlist(objective$data_1$continous)]))
    con_varcovar2 <- measure_arg_con[[arg]](data.frame(dat2[, unlist(objective$data_2$continous)]))
    diff_result_con <- append(diff_result_con , list(arg = list(con_varcovar1, con_varcovar1)))
  }
  names(diff_result_con) <- names(measure_arg_con)
  abc1 <- function(){
    diff_result_cat <- NULL
    for (arg in names(measure_arg_cat)) {
      #arg = table
      cat_table1 <- measure_arg_cat[[arg]](data.frame(dat1[, unlist(objective$data_1$categorical), drop=F]))
      cat_table2 <- measure_arg_cat[[arg]](data.frame(dat2[, unlist(objective$data_2$categorical), drop=F]))

      table(dat1[, unlist(objective$data_1$categorical), drop=F], dat2[, unlist(objective$data_2$categorical), drop=F])
    }

    colnames(diff_result_cat) <- c(unlist(objective$data_2$categorical), "data1", "data2")
    diff_result_bin <- NULL
    for (arg in names(measure_arg_bin)) {
      #arg = "table"
      bin_table1 <- data.frame(apply(dat1[, unlist(objective$data_1$binary), drop=F], 2, measure_arg_bin[[arg]]))
      bin_table2 <- data.frame(apply(dat2[, unlist(objective$data_2$binary), drop=F], 2, measure_arg_bin[[arg]]))
      abc <- merge(bin_table1, bin_table2, by="row.names")
      diff_result_bin <- rbind(diff_result_bin, abc)
    }
    colnames(diff_result_bin) <- c(unlist(objective$data_2$binary), "data1", "data2")
  }
  #, result_cat = diff_result_cat, result_bin = diff_result_bin



   return(list(result_con = diff_result_con))
}

