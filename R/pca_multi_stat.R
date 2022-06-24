#' obtain and compare custom summary statistics from given two datasets
#'
#' @description obtain and compare custom summary statistics from given two datasets. The comparison are made on three main aspects : continuous, categorical and binary
#' @import ggplot2
#' @param dat1 First Data need to be compared.
#' @param dat2 Second Data need to be compared.
#' @param key Unique identifiers to link two datasets
#' @param ddiff_objective First Data need to be compared.
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

pca_multi_stat <- function(dat1, dat2, key, ddiff_objective, ...){
  abc <- function(...){
    library(ddiff)
    d = generate_test_data(50)
    dat2 = d$records_added$new
    dat1 = d$records_added$old
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
  objective = get_variable_class(dat1, dat2, "id")
  dat1_svd = svd(as.data.frame(ddiff_objective$result_con$cov[1]))
#  library(ggplot2)
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }
  percent_pca = percent(dat1_svd$d/sum(dat1_svd$d))
  project_data1 = data.matrix(dat1[, unlist(objective$data_1$continous)])%*%dat1_svd$u[,c(1,2)]
  project_data2 = data.matrix(dat2[, unlist(objective$data_2$continous)])%*%dat1_svd$u[,c(1,2)]
  library(ggplot2)
  p = ggplot(as.data.frame(project_data1), aes(x=project_data1[,1], y=project_data1[,2], colour = "data1")) +
  xlab(paste0("pc1(", percent_pca[1], ")")) + ylab(paste0("pc2(", percent_pca[2], ")")) + geom_point(size = 5) +
  geom_point(aes(x = project_data2[,1], y = project_data2[,2], colour = "data2"), as.data.frame(project_data2), size = 3) +
  scale_colour_manual(name="legend", values = c(data1 = "black", data2 = "red")) + ggtitle("samples from data2 projected on data1 PC 1,2 space")

  return(p)
}
