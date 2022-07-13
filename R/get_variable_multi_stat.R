#' obtain and compare custom multivariate summary statistics from given two datasets
#'
#' @description obtain and compare custom multivariate summary statistics from given two datasets.
#' @param dat1 First Data need to be compared.
#' @param dat2 Second Data need to be compared.
#' @param key Unique identifiers to link two datasets
#' @param measure_arg_con list of function for summary statistics of continuous response. The defalut list is list(cov = cov)
#' @param measure_arg_cat list of function for summary statistics of categorical response
#' @param measure_arg_bin list of function for summary statistics of binary response
#' @param dis_arg list of distance for measuring the difference of variables in each datasets
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
                          measure_arg_cat = list(table = table), measure_arg_bin = list(table = table), dis_arg = list("euclidean", "manhattan", "gower"), ...){
  abc <- function(...){
    library(ddiff)
    d = generate_test_data(100)
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
    abcd = rbind(dat1[, unlist(objective$data_1$continous)[1]], dat2[, unlist(objective$data_2$continous)[1]])
    #gower distance
    library(gower)
    gower_dist(t(dat1), t(dat2))
    edit(gower_dist)
    ?gower_work
    ??gower_work

    library(StatMatch)

    x1 <- as.logical(rbinom(10,1,0.5))
    x2 <- sample(letters, 10, replace=TRUE)
    x3 <- rnorm(10)
    x4 <- ordered(cut(x3, -4:4, include.lowest=TRUE))
    xx <- data.frame(x1, x2, x3, x4, stringsAsFactors = FALSE)

    # matrix of distances between observations in xx
    dx <- gower.dist(xx)
    head(dx)

    # matrix of distances between first obs. in xx
    # and the remaining ones
    abc <- gower.dist(data.x= dat1, data.y= dat2)
    abc
    rowMeans(abc[101:200, 1:100])
    (rowMeans(abc[101:200, 1:100]) - mean(rowMeans(abc[101:200, 1:100])) )/ sum(rowMeans(abc[101:200, 1:100]))
    abc <- gower.dist(data.x= t(dat1[sample(1:200, 100), ]), data.y= t(dat2))
    abc
    abc <- gower.dist(data.x= t(dat2), data.y= t(dat2))
    abc
    #diagonal

    #continuous distance
    comp.cont(data.A = dat1, data.B = dat2, xlab.A = "nm_1")
    #categorical distance
    comp.prop(p1=table(dat1$cat_1), p2=table(dat2$cat_1), n1=nrow(dat1), n2=nrow(dat2), ref=FALSE)
  }

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

  #, result_cat = diff_result_cat, result_bin = diff_result_bin
   return(list(result_con = diff_result_con))
}

