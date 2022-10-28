
get<- function(diffable_obj, element, ...){
  UseMethod(generic = "get",object = diffable_obj)
}


#' @export
get.diffable <- function(diffable_obj, element, include_metadata_cols = F){
  # browser()
  # exists(include_metadata_cols)
  valid_elements <- c("df",
                      # "df_new_records",
                      "df_ref","signature","signature_ref",
                      "key","col_types","col_names","attr_match","records_matched")
  
  # browser()
  if(length(element)>1)
    warning(cli::format_warning("All but the 1st element is ignored"))
  
  element <- element[1]
  
  if(!element %in% valid_elements)
    stop(cli::format_error("{element} is not valid. Pick one of
                  {paste0(valid_elements,collapse = \", \")}"))
  
  
  if(element %in% names(diffable_obj))
    out <- diffable_obj[[element]]
  
  # if(element == "df_new_records"){
  #  out <-  diffable_obj$records_matched %>% 
  #     dplyr::filter(record_match_status %in% c("modified_to","added")) %>%
  #     dplyr::select(colnames(diffable_obj$df))
  # }
  
  if(element == "col_names"){
    out <- names(diffable_obj$col_types$all)
  }
  
  
  if(!include_metadata_cols){
    if(element %in% c("df", 
                      # "df_new_records"
                      "df_ref"))
      out <- out %>% `rownames<-`(.$rownames) %>%
        dplyr::select(-c(record_sig,rownames))
  }
  
  return(out)
  
}
