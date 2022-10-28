#' obtain types of variables from given two datasets
#'
#' @description This function could obtain types of variables from given two datasets. Note that types of variables are defined as follows : continuous, categorical and binary
#' @param df_ref reference data.frame relative to which comparison is made.
#' @param df a data.frame compared to the refernce
#' @param key Unique identifiers to link two datasets
#' @param ... potential argument to added
#' @return The types of variables from given two datasets.
#' @examples
#' d = test_data()
#' ddiff_rpt <- ddiff_class(d$identical$new, d$identical$old, c("ID", "Days"))
#' data.tree::FromListSimple(ddiff_rpt)
#' str(ddiff_rpt)
#' @export
#'
ddiff_class <- function(df_ref, df, key = character(0), ...){
  
  if(!is.data.frame(df)){
    er <- "df provided is not a data.frame"
    stop(cli::format_error(er))
  }
  
  if(!is.data.frame(df_ref)){
    er <- "df_ref provided is not a data.frame"
    stop(cli::format_error(er))
  }
  
  
  missing_keys <- setdiff(key,colnames(df))
  if(length(missing_keys)>0){
    er <- glue::glue("key set {paste0(missing_keys, collapse = \", \")} missing from df")
    stop(cli::format_error(er))
  }
  
  missing_keys <- setdiff(key,colnames(df_ref))
  if(length(missing_keys)>0){
    er <- glue::glue("key set {paste0(missing_keys, collapse = \", \")} missing from df_ref")
    stop(cli::format_error(er))
  }
  
  
  df_class <- guess_col_class(df)
  df_ref_class <- guess_col_class(df_ref)
  
  cols_mismatch <- setdiff(union(names(df_class),names(df_ref_class)), 
                           intersect(names(df_class),names(df_ref_class)))
  
  
  if(length(cols_mismatch)>0){
    er <- glue::glue("colnames {paste0(cols_mismatch, collapse = \", \")} ",
                     "don't match across the two data.frames")
    custom_err(subclass = "mismatch_cols", message = er)
  }
  
  # TODO: make the error message more specific showing which cols have mismatch
  # classes
  if(!all(df_class == df_ref_class[names(df_class)])){
    er <- "classes don't match"
    custom_err(subclass = "mismatch_cols_class", message = er)
  }
  
  diff_objects <- list(df = df, df_ref = df_ref, key = key,
                       col_type = df_class)
  
  invisible(diff_objects)
}