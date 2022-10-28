#' @title Constructor for condition
#' @author Adopted from Hadley Wickham
#' @description Convenience function to construct condition
#' @param subclass a character name of the custom error class e.g. my_error
#' @param message a message to be associated with the custom error type
#' @param call system call that generated the error
#' @return condition
#' @keywords internal
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call),
    ...
  )
}

#' @title Assert for condition
#' @author Adopted from Hadley Wickham
#' @description Asserts that x is a condition
#' @param x Condition 
#' @return TRUE or FALSE
#' @keywords internal
is.condition <- function(x) inherits(x, "condition")


#' @title Generate (sub)class-specific error
#' @author Adopted from Hadley Wickham
#' @description This will allow creation of a custom error of type `<subclass>`.
#' which inherits from type `error`. This enables setting up `tryCatch` 
#' statements that could behave differently based on error type. (see 
#' http://adv-r.had.co.nz/Exceptions-Debugging.html)
#' @param subclass a character name of the custom error class e.g. my_error
#' @param message a message to be associated with the custom error type
#' @param call system call that generated the error
#' @example 
#' tryCatch(expr = custom_err(subclass = "foo",message = "this is a message"),
#'          invalid_class = function(cond){2+2},
#'          invalid_value = function(cond) "value",
#'          foo = function(cond){
#'          message("foo type error was caught with following message")
#'            cat(cond$message,"\n")
#'            return("bar")})
#' @keywords internal
custom_err <- function(subclass, message, call = sys.call(-1), 
                           ...) {
  message <- cli::format_error(message)
  er_obj <- condition(c(subclass, "error"), message, call = call, ...)
  stop(er_obj)
}

#' @title  Get cryptographic signature for a table
#' @description Given a data.frame, it gets cryptographic signature for the 
#' table when written on disk as a parquet file
#' @param df a data.frame
#' @param pqrquet_v the version of parquet to be used for getting the signature
#' @param ignore_col column names to disregard
#' @return a list with signature and parquet version used
dfsig_get <- function(df, parquet_v = "2.4", ignore_col = "rownames"){
  df <- df[,setdiff(colnames(df),ignore_col), drop = F]
  tf <- tempfile(fileext = ".parquet")
  arrow::write_parquet(x = df,sink = tf, version = parquet_v)
  dfsig <- digest::digest(tf, algo="sha1", serialize=FALSE, file=TRUE)
  unlink(tf)
  return(list(dfsig = dfsig, parquet_v = parquet_v))
}


is.diffable <- function(diffable_obj){
  return("diffable" %in% class(diffable_obj))
}

colnames_match <- function(diffable_obj){
  if(!is.diffable(diffable_obj))
    stop(cli::format_error("object is not of class diffable"))
  
  return(diffable_obj$attr_match$colnames_match)
}

rownames_match <- function(diffable_obj){
  if(!is.diffable(diffable_obj))
    stop(cli::format_error("object is not of class diffable"))
  
  return(diffable_obj$attr_match$rownames_match)
}

attrnames_match <- function(diffable_obj){
  if(!is.diffable(diffable_obj))
    stop(cli::format_error("object is not of class diffable"))
  
  return(diffable_obj$attr_match$attrnames_match)
}


other_attrs_match <- function(diffable_obj){
  if(!is.diffable(diffable_obj))
    stop(cli::format_error("object is not of class diffable"))
  
  return(diffable_obj$attr_match$other_attrs_match)
}

d_match<- function(diffable_obj){
  browser()
  if(!is.diffable(diffable_obj))
    stop(cli::format_error("object is not of class diffable"))
  if(diffable_obj$sig$df$parquet_v == diffable_obj$sig$df_ref$parquet_v)
    return(diffable_obj$sig$df$dfsig == diffable_obj$sig$df_ref$dfsig)
  
  warning(cli::format_warning("parquet version for signatures didn't match"))
  return(isTRUE(all.equal(diffable_obj$df_ref, diffable_obj$df,
                          check.attributes = F)))
}


add_record_sig <- function(df,
                           ignore_col = "rownames",
                           funsig = function(x){
                             digest::digest(x, algo = "sha1", serialize = T,
                                            file = F)}){
  df0 <- df
  df_rownames <- df$rownames
  df <- df[,setdiff(colnames(df),ignore_col), drop = F]
  df <- sapply(df, format, trim = T)
  df_sig_added <- cbind(rownames = df_rownames, 
                        record_sig = apply(X = df, MARGIN = 1, FUN = function(x) 
    funsig(x)), df0[,setdiff(colnames(df),ignore_col), drop = F])
  
  return(df_sig_added)
}

jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}