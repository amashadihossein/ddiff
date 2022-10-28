#' @title Attributes diff
#' @description Compares attribues by checking colnames set and rownames set as 
#' well as other attributes and whether they are all the identical
#' @param df_ref reference data.frame relative to which comparison is made.
#' @param df a data.frame compared to the reference
#' @return a data.frame of logical with columns `colnames_match`, 
#' `rownames_match`, `attrnames_match`, and `other_attrs_match`
#' @examples
#' diff_attr(cars,cars)
#' @keywords internal
diff_attr <- function(df, df_ref){
  attrs_ref <- attributes(df_ref)
  attrs_new <- attributes(df)
  
  attr_names <- list(ref = names(attrs_ref),
                     new = names(attrs_new))
  
  attr_checklist <- list(colnames_match = NA, rownames_match = NA,
                         attrnames_match = NA, other_attrs_match = NA)
  
  attr_checklist$colnames_match <-  
    length(setdiff(colnames(df_ref), colnames(df))) == 0 &
    ncol(df_ref) == ncol(df)
  
  if("row.names" %in% intersect(attr_names$new, attr_names$ref)){
    attr_checklist$rownames_match <-  
      length(setdiff(rownames(df_ref), rownames(df))) == 0 &
      nrow(df_ref) == nrow(df)
  }
  
  attr_checklist$attrnames_match <- 
    length(setdiff(attr_names$ref, attr_names$new)) == 0 &
    length(attr_names$ref) == length(attr_names$new)
  
  
  if(!attr_checklist$attrnames_match){
    return(as.data.frame(attr_checklist))
  }

  
  attrs_to_compare <- setdiff(attr_names$new,c("names","row.names"))
  
  if(length(attrs_to_compare) > 0){
    attr_checklist$other_attrs_match <-
      all(vapply(attrs_to_compare, 
                 function(attr_i) identical(attrs_new[[attr_i]], attrs_ref[[attr_i]]), 
                 FUN.VALUE = F))
  }

  
  return(as.data.frame(attr_checklist))
}