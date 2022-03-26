ddiff <- function(d_new, d_old){
  # cat("z")
  
  d_old <- d_old0 <- tibble::as_tibble(d_old)
  d_new <- d_new0 <- tibble::as_tibble(d_new)
  
  ddiff_obj <- list()
  ddiff_obj$identical <- ddiff_obj$equivalent <- ddiff_obj$attrnames_match <- ddiff_obj$d_match <- ddiff_obj$comparable <- F
  ddiff_obj$message <- character(0)
  ddiff_obj$comp_obj <- NA
  
  if(identical(x = d_new, y = d_old)){
    ddiff_obj$identical <- ddiff_obj$equivalent <- ddiff_obj$attrnames_match <- ddiff_obj$d_match <- ddiff_obj$comparable <- T
    ddiff_obj$message <- "Data tables are identical"
    return(ddiff_obj)
  }
    
  
  attr_checked <- attr_check(d_new = d_new, d_old = d_old)
  
  if(!attr_checked$cols_match){
    ddiff_obj$message <- "Colnames are different. Ensure d_new and d_old being compared have the same colnames"
    return(ddiff_obj)
  }
  
  #TODO: look into hash by row to see if we can better match rows
  ddiff_obj$comparable  <- attr_checked$cols_match
  d_new <- d_new[,match(colnames(d_old), colnames(d_new))]
  d_old <- dplyr::arrange(.data = d_old, dplyr::across())
  d_new <- dplyr::arrange(.data = d_new, dplyr::across())
  ddiff_obj$d_match <- isTRUE(all.equal(d_old, d_new, check.attributes = F))
  ddiff_obj$attrnames_match <- attr_checked$attrnames_match
  
  #check if equivalent
  if(attr_checked$attrnames_match){
    ddiff_obj$equivalent <- identical(x = d_new, y = d_old)
  }else{
    new_attrnames <- names(attributes(d_new0))
    old_attrnames <- names(attributes(d_old0)) 
    
    if(length(added_attrs <- setdiff(new_attrnames, old_attrnames))>0)
      ddiff_obj$message <- paste0("Attributes added ",paste0(added_attrs,collapse = ", "),"\n")
    
    if(length(removed_attrs <- setdiff(old_attrnames, new_attrnames))>0)
      ddiff_obj$message <- paste0(ddiff_obj$message,"Attributes removed",paste0(removed_attrs,collapse = ", "),"\n")
  }
    
  
  if(ddiff_obj$equivalent){
    ddiff_obj$message <- "Data table contents and attr names match: differences are limited to row or col orders or other attribute values"
    return(ddiff_obj)
  }
  
  if(ddiff_obj$d_match){
    ddiff_obj$message <- paste0("Data table contents match but attr names may differ\n", ddiff_obj$message,"\n")
    return(ddiff_obj)
  }
  
  ddiff_obj$message <- paste0(ddiff_obj$message,"See comp_obj for detail comparison","\n")
  ddiff_obj$comp_obj <- compareDF::compare_df(df_new = d_new0, df_old = d_old0)

  return(ddiff_obj)
}