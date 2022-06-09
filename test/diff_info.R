ddiff2 <- function(dat1, dat2, key, ...){
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
}
