#' compare given two datasets in form level
#'
#' @description This function could obtain and compare empirical information of row, column and attributes from given two datasets. The comparison are made on following aspects :
#' 1. whether the types of variables from given two datasets are the same
#' 2. whether column names from given two datasets are the same
#' 3. whether row names from given two datasets are the same
#' 4. whether attributes from given two datasets are the same
#' 5. whether the information given two datasets are the same (shuffled rows or columns)
#' 6. whether the given two datasets are equivalent in R memory
#' @param dat1 First Data need to be compared.
#' @param dat2 Second Data need to be compared.
#' @param key Unique identifiers to link two datasets
#' @param ... potential argument to added
#' @return ddiff which contains indicator of above aspects. Also, the last element include existing comparedf object.
#'
#' @examples
#' d <- test_data()
#' ddiff_rpt <- ddiff_form(d$identical$new, d$identical$old, c("ID", "Days"))
#' ddiff_rpt
#' @export
#'
ddiff_form <- function(dat1, dat2, key, ...){
  #key = "id"
  dat1$id <- tidyr::unite(as.data.frame(dat1[,key]),  key, remove = FALSE, na.rm = FALSE)$key
  dat2$id <- tidyr::unite(as.data.frame(dat2[,key]),  key, remove = FALSE, na.rm = FALSE)$key
  n1 = nrow(dat1); d1 = ncol(dat1); n2 = nrow(dat2); d2 = ncol(dat2);
  ddiff_obj <- list()
  #ddiff_obj$identical <- ddiff_obj$info_match <- ddiff_obj$attributes_match <- ddiff_obj$row_match <- ddiff_obj$column_match <- F
  ddiff_obj$info_match <- ddiff_obj$row_match <- ddiff_obj$column_match <- ddiff_obj$attributes_match <- ddiff_obj$identical <- T
  ddiff_obj$message <- paste0( "data1 and data2 are identical")
  if(class(dat1) != "data.frame"  | class(dat2) != "data.frame"){
    return("error : input must be data frame objects")
  }
 #class
 # if(sum(apply(dat1, 2, class) == apply(dat2, 2, class)[data.table::chmatch(colnames(dat1), colnames(dat2))]) == d1){
 #   ddiff_obj$class_match <- T
 #   ddiff_obj$message <- paste0("variable class of ", deparse(substitute(dat1)), " and ",  deparse(substitute(dat2)), " are identical")
 # }

  #row shuffled or column shuffled
  if(!identical(x = dat1[data.table::chmatch(dat2[,"id"], dat1[,"id"]), data.table::chmatch(colnames(dat2), colnames(dat1))], y = dat2)){
    ddiff_obj$info_match <- F
  }

  ##rownames different (unique identifiers case (user provided))
  row_match = sum(data.table::chmatch(dat2[,"id"], dat1[,"id"]) -  seq(1, n2, 1)) != 0
  if(row_match & !is.na(row_match)){
    ddiff_obj$row_match <- F
  }

  #colnames different
  if(sum(sort(data.table::chmatch(colnames(dat2), colnames(dat1))) - seq(1, d2, 1)) != 0){
    ddiff_obj$column_match <- F
  }

  #attributes different
  attributes_match = data.table::chmatch(names(attributes(dat1)), names(attributes(dat2)))
  if(is.na(sum(attributes_match)) | length(names(attributes(dat1))) != length(names(attributes(dat2)))){
    ddiff_obj$attributes_match <- F
  }

  #identical
  if(!identical(x = dat1, y = dat2)){
    ddiff_obj$identical <- F
  }

  if(ddiff_obj$info_match & !ddiff_obj$identical){
    ddiff_obj$message <- "Data table contents and attr names match: differences are limited to row or col orders"
  }
  if(ddiff_obj$column_match & !ddiff_obj$row_match){
    ddiff_obj$message <- paste0("row identifier of data1 and data2 are different")
  }
  if(!ddiff_obj$column_match & ddiff_obj$row_match){
    ddiff_obj$message <- paste0("Column names of data1 and data2 are different")
  }
  if(!ddiff_obj$attributes_match){
    ddiff_obj$message <- paste0("attributes of data1 and data2 are different")
  }
  if(!ddiff_obj$attributes_match & !ddiff_obj$info_match & !ddiff_obj$column_match & !ddiff_obj$row_match & !ddiff_obj$identical)
  ddiff_obj$message <- paste0( "data1 and data2 are different in all aspects")

  ddiff_obj$comparedf <- arsenal::comparedf(dat1, dat2)
  return(ddiff_obj)
}
