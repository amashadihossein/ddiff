ddiff <- function(df, df_ref, key){
  
  df_ref0 <- df_ref #<- tibble::as_tibble(df_ref)
  df0 <- df #<- tibble::as_tibble(df)
  
  ddiff_obj <- list()
  
  ddiff_obj$comp_obj <- ddiff_obj$identical <- ddiff_obj$equivalent <- 
    ddiff_obj$attrnames_match <- ddiff_obj$d_match <- ddiff_obj$comparable <- NA
  ddiff_obj$message <- "The data tables are not comparable"
  
  # Check if diffable (comparable)
  diffable_obj <- tryCatch(expr = diffable(df = df, df_ref = df_ref),
                           mismatch_cols = function(cond){
                             message(cli::format_message("Columns need to match for diff to be possible:"))
                             cat(cond$message,"\n")
                           },
                           mismatch_cols_class = function(cond){
                             message(cli::format_message("Column class need to match for diff to be possible:"))
                             cat(cond$message,"\n")
                           },
                           error = function(cond){
                             cat(cond$message,"\n")
                           })
  
  if(!is.diffable(diffable_obj)){
    ddiff_obj$comp_obj <- F
    return(ddiff_obj)
  }
    
    
  # Check if identical
  if(identical(x = df, y = df_ref)){
    ddiff_obj$identical <- ddiff_obj$equivalent <- ddiff_obj$attrnames_match <- 
      ddiff_obj$d_match <- ddiff_obj$comparable <- T
    ddiff_obj$message <- "Data tables are identical"
    return(ddiff_obj)
  }
  ddiff_obj$identical <- F

  
  #TODO: look into hash by row to see if we can better match rows
  ddiff_obj$comparable  <- colnames_match(diffable_obj)
  ddiff_obj$d_match <- d_match(diffable_obj)
  ddiff_obj$attrnames_match <- attrnames_match(diffable_obj)
  
  #check if equivalent
  if(ddiff_obj$attrnames_match){
    ddiff_obj$equivalent <- identical(x = df, y = df_ref)
  }else{
    new_attrnames <- names(attributes(df0))
    old_attrnames <- names(attributes(df_ref0)) 
    
    if(length(added_attrs <- setdiff(new_attrnames, old_attrnames))>0)
      ddiff_obj$message <- paste0("Attributes added ", 
                                  paste0(added_attrs,collapse = ", "),"\n")
    
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
  
  if(length(get(diffable_obj,element = "key")) > 0){
    ddiff_obj$comp_obj <- compareDF::compare_df(df_new = df0, df_old = df_ref0, 
                                                group_col = key)
  }else{
    ddiff_obj$comp_obj <- compareDF::compare_df(df_new = df0, df_old = df_ref0)
  }

  
  return(ddiff_obj)
}