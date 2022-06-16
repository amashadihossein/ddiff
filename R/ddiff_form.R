#' obtain and compare empirical information of row, column and attributes from given two datasets
#'
#' @description obtain and compare empirical information of row, column and attributes from given two datasets. The comparison are made on following aspects :
#' 1. whether the types of variables from given two datasets are the same
#' 2. whether column names from given two datasets are the same
#' 3. whether row names from given two datasets are the same
#' 4. whether attributes from given two datasets are the same
#' 5. whether the information given two datasets are the same (shuffled rows or columns)
#' 6. whether the given two datasets are equivalent in R memory
#' @param dat1 First Data need to be compared.
#' @param dat2 Second Data need to be compared.
#' @param key Unique identifiers to link two datasets
#' @return ddiff_obj_info which contains indicator of above aspects. Also, the last element include existing comparedf object.
#' @examples
#' #result <- ddiff_form(d$identical$new, d$identical$old, "id")
#' #result$class_match
#' #result$column_match
#' #result$row_match
#'
#'
ddiff_form <- function(dat1, dat2, key, ...){
  n1 = nrow(dat1); d1 = ncol(dat1); n2 = nrow(dat2); d2 = ncol(dat2)
  ddiff_obj <- list()
  ddiff_obj$identical <- ddiff_obj$info_match <- ddiff_obj$attributes_match <- ddiff_obj$row_match <- ddiff_obj$column_match <- ddiff_obj$class_match <- F
  if(class(dat1) != "data.frame"  | class(dat2) != "data.frame"){
    return("error : input must be data frame objects")
  }

  #identical
  if(identical(x = dat1, y = dat2)){
    ddiff_obj$identical <- T
    ddiff_obj$message <- paste0(deparse(substitute(dat1)), " and ",  deparse(substitute(dat2)), " are identical")
  }

  #attributes different
  attributes_match = data.table::chmatch(names(attributes(dat1)), names(attributes(dat2)))
  if(!is.na(sum(attributes_match)) & length(names(attributes(dat1)) == length(names(attributes(dat2))))){
    ddiff_obj$attributes_match <- T
    ddiff_obj$message <- paste0("attributes of ", deparse(substitute(dat1)), " and ",  deparse(substitute(dat2)), " are different")
  }

  #colnames different
  if(sum(sort(data.table::chmatch(colnames(dat1), colnames(dat2))) - seq(1, d2, 1)) == 0){
    ddiff_obj$column_match <- T
    ddiff_obj$message <- paste0("Column of ", deparse(substitute(dat1)), " and ",  deparse(substitute(dat2)), " are different")
  }

  ##rownames different (unique identifiers case (user provided))
  #dat1 = d$records_added$new
  #dat2 = d$records_added$old
  #key = "id"
  #n1 = nrow(dat1); d1 = ncol(dat1); n2 = nrow(dat2); d2 = ncol(dat2)
  row_match = sum(data.table::chmatch(dat1[,key], dat2[,key]) -  seq(1, n2, 1)) == 0
  if(row_match & !is.na(row_match)){
    ddiff_obj$row_match <- T
    ddiff_obj$message <- paste0("row of ", deparse(substitute(dat1)), " and ",  deparse(substitute(dat2)), " are different")
  }

  #row shuffled or column shuffled
  if(identical(x = dat1[data.table::chmatch(dat2[,key], dat1[,key]), data.table::chmatch(colnames(dat2), colnames(dat1))], y = dat2)){
    ddiff_obj$info_match <- T
    ddiff_obj$message <- "Data table contents and attr names match: differences are limited to row or col orders"
  }

  ddiff_obj$comparedf <- arsenal::comparedf(dat1, dat2)
  return(ddiff_obj)

  #information content diff
  ddiff_obj_info <- list()
  sapply(dat1, class)
  class(dat1$nm_1)
  str(dat1)
}
