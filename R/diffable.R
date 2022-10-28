#' @title diffable
#' @description Creates a data object that valid for ddiff operation
#' @param df_ref reference data.frame relative to which comparison is made.
#' @param df a data.frame compared to the reference
#' @param key Unique identifiers to link two datasets
#' @param ... potential argument to added
#' @return The types of variables from given two datasets.
#' @example 
#' old <- mtcars[1:6,]
#' new <- mtcars[3:8,]
#' new <- new[rev(1:nrow(new)),]
#' old$carb[4]<-NA
#' old$cyl[6]<-12
#' diffable_obj <- diffable(df = new,df_ref = old)
#' get(diffable_obj, "records_matched")
#' @export
diffable <- function(df, df_ref, key = character(0), ...){
  
  reserved_names <- c("rownames","rowsig_index", "record_sig", "in_df_ref",
                      "in_df", "maps_to", "record_match_status")
  
  if(!is.data.frame(df)){
    er <- "df provided is not a data.frame"
    stop(cli::format_error(er))
  }
  
  
  if(!is.data.frame(df_ref)){
    er <- "df_ref provided is not a data.frame"
    stop(cli::format_error(er))
  }
  
  reserved_names_used <- intersect(reserved_names, colnames(df))
  if(length(reserved_names_used) > 0){
    er <- glue::glue("The following reserved names were found in the df.
                     {paste0(reserved_names_used, collapse = \", \")}
                     Please rename and retry")
  }
  
  
  reserved_names_used <- intersect(reserved_names, colnames(df_ref))
  if(length(reserved_names_used) > 0){
    er <- glue::glue("The following reserved names were found in the df_ref.
                     {paste0(reserved_names_used, collapse = \", \")}
                     Please rename and retry")
  }
  
  
  missing_keys <- setdiff(key,colnames(df))
  if(length(missing_keys)>0){
    er <- glue::glue("key set {paste0(missing_keys, collapse = \", \")} \\
                     missing from df")
    stop(cli::format_error(er))
  }
  
  missing_keys <- setdiff(key,colnames(df_ref))
  if(length(missing_keys)>0){
    er <- glue::glue("key set {paste0(missing_keys, collapse = \", \")} \\
                     missing from df_ref")
    stop(cli::format_error(er))
  }
  
  # browser()
  attr_match <- diff_attr(df = df,df_ref = df_ref)
  
  if(!attr_match$colnames_match){
    cols_mismatch <- setdiff(union(colnames(df),colnames(df_ref)), 
                             intersect(colnames(df),colnames(df_ref)))
    er <- glue::glue("
    To be diffable, the colnames should match!
    Colnames {paste0(cols_mismatch, collapse = \", \")} are not shared across \\
    the two data.frames.
                     ")
    custom_err(subclass = "mismatch_cols", message = er)
  }
  
  df_class <- guess_col_class(df)
  df_ref_class <- guess_col_class(df_ref)
  
  df_ref <- df_ref %>% dplyr::mutate(rownames = rownames(.)) %>%
    dplyr::arrange(dplyr::across())
  
  df <- df %>%  dplyr::mutate(rownames = rownames(.)) %>%
    dplyr::select(match(colnames(df_ref), colnames(.))) %>%
    dplyr::relocate(rownames) %>%
    dplyr::arrange(dplyr::across())
  
  
  # TODO: make the error message more specific showing which cols have mismatch
  # classes
  if(!all(df_class$all == df_ref_class$all[names(df_class$all)])){
    er <- "classes don't match"
    custom_err(subclass = "mismatch_cols_class", message = er)
  }
  
  # get signature
  sig <- list()
  sig$df <- dfsig_get(df = df, ignore_col = "rownames")
  sig$df_ref <- dfsig_get(df = df_ref, ignore_col = "rownames")
  
  # Add record_sig
  df <- add_record_sig(df = df, ignore_col = "rownames")
  df_ref <- add_record_sig(df = df_ref, ignore_col = "rownames")
  
  
  
  diffable_obj <- list(df = df,
                           df_ref = df_ref,
                           key = key,
                           sig = sig,
                           col_types = df_class,
                           # rows_unchanged = ,
                           # rows_modified =,
                           # rows_removed =,
                           attr_match = attr_match)
  
  class(diffable_obj) <- c("diffable", class(diffable_obj))
  
  
  diffable_obj$records_matched <- match_record(diffable_obj, mincol_match_rate = 0.5)
  
  
  #"df"         "df_ref"     "key"        "sig"        "col_type"   "attr_match"
  invisible(diffable_obj)
}