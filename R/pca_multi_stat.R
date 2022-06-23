#' obtain and compare custom summary statistics from given two datasets
#'
#' @description obtain and compare custom summary statistics from given two datasets. The comparison are made on three main aspects : continuous, categorical and binary
#' @param dat1 First Data need to be compared.
#' @param dat2 Second Data need to be compared.
#' @param key Unique identifiers to link two datasets
#' @param measure_arg_con list of function for summary statistics of continuous response. The defalut list is c("min", "max", "median", "mean", "standard error")
#' @param measure_arg_cat list of function for summary statistics of categorical response
#' @param measure_arg_bin list of function for summary statistics of binary response
#' @param ... potential argument to added
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' n = 100
#' d = generate_test_data(n)
#' #result = get_variable_stat(d$identical$new, d$identical$old, "id")
#' #result$result_con
#' #result$result_cat
#' #result$result_bin
#' @export
#'

pca_multi_stat <- function(ddiff_objective, ...){
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
    ddiff_objective = get_variable_multi_stat(d$records_added$new, d$records_added$old, "id")
  }
  dat1_svd = svd(as.data.frame(ddiff_objective$result_con$cov[1]))

  project_data1 = data.matrix(dat1[, unlist(objective$data_1$continous)])%*%dat1_svd$u[,c(1,2)]
  project_data2 = data.matrix(dat2[, unlist(objective$data_2$continous)])%*%dat1_svd$u[,c(1,2)]
  plot(project_data1[,1], project_data1[,2], col = "red", pch = 15, xlab = c("pca (1st component)"), ylab = c("pca(2nd component)"))
  points(project_data2[,1], project_data2[,2], col="blue", pch = 17)
  legend(1, 95, legend=c("Old", "New"), col=c("red", "blue"), pch=c(15,17), cex=0.8)
  return(list(result_con = diff_result_con))
}
