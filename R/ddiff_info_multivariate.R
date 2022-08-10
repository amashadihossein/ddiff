#' obtain and compare custom multivariate summary statistics from given two datasets
#'
#' @description This function could obtain and compare custom multivariate 
#' summary statistics from given two datasets. Specially, the concurrent version
#'  involves obtaining the variance-covariance matrix of given two datasets
#' @param dat1 First Data need to be compared.
#' @param dat2 Second Data need to be compared.
#' @param key Unique identifiers to link two datasets
#' @param key_ex variables that is excluded from variance covariance estimation
#' @param measure_arg_con list of function for summary statistics of continuous 
#' response. The defalut list is list(cov = cov)
#' @param measure_arg_cat list of function for summary statistics of categorical
#'  response
#' @param measure_arg_bin list of function for summary statistics of binary response
#' @param dis_arg list of distance for measuring the difference of variables in 
#' each datasets
#' @param ... potential argument to added
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' d <- test_data()
#' result <- ddiff_info_multivariate(dat1 = d$identical$new,
#'  dat2 = d$identical$old, key = c("ID", "Days"),
#'  key_ex =  c("nEggs" , "nEggsRemain"))
#' result$result_con
#' result$result_cat
#' result$result_bin
#' @export
#'
ddiff_info_multivariate <- function(dat1, dat2, key, key_ex,
                                    measure_arg_con = list(cov = cov),
                                    measure_arg_cat = list(table = table), measure_arg_bin = list(table = table), dis_arg = list("euclidean", "manhattan", "gower"), ...){
  objective = ddiff_class(dat1, dat2, key)
  diff_result <- NULL
  diff_result_con <- NULL
  key1 = cbind(key, key_ex)
  for (arg in names(measure_arg_con)) {
    names1 = unlist(objective$data_1$continous)
    names2 = unlist(objective$data_2$continous)
    con_varcovar1 <- measure_arg_con[[arg]](data.frame(dat1[, names1[! names1 %in% key1]]))
    con_varcovar2 <- measure_arg_con[[arg]](data.frame(dat2[, names2[! names2 %in% key1]]))
    diff_result_con <- append(diff_result_con , list(arg = list(con_varcovar1, con_varcovar1)))
  }
  names(diff_result_con) <- names(measure_arg_con)

  return(list(result_con = diff_result_con))
}
