#' @title Guess column class
#' @description Given a data.frame it guess its column classes as `continuous`,
#'  `categorical`, `binary` or `date`
#' @param a data.frame
#' @example guess_col_class(cars)
#' @keywords internal
guess_col_class <- 
  function(df, 
           guess_fun = function(x) readr::guess_parser(as.character(x))){
    
  if(!is.data.frame(df)){
    er <- "df provided is not a data.frame"
    stop(cli::format_error(er))
  }
  
  col_class_guessed <- vapply(as.list(df), guess_fun , c("class" = "class"))
  cols_nlevels <- vapply(as.list(df),
                              FUN = function(x) length(unique(x)),
                              FUN.VALUE = c("nlevel" = 0))
  
  bin_cols <- names(cols_nlevels)[cols_nlevels==2]
  
  # TODO: consider other possible guess types such as number and date and how to
  # handle
  col_class_guessed[col_class_guessed %in% c("double")] <- "continuous"
  col_class_guessed[col_class_guessed == "character"] <- "categorical"
  col_class_guessed[names(col_class_guessed) %in% bin_cols] <- "binary"
  
  all_cols <- col_class_guessed
  
  col_class_guessed <- list()
  col_class_guessed$continuous <- names(all_cols)[all_cols == "continuous"]
  col_class_guessed$binary <- names(all_cols)[all_cols == "binary"]
  col_class_guessed$categorical <- names(all_cols)[all_cols == "categorical"]
  col_class_guessed$date <- names(all_cols)[all_cols == "date"]
  col_class_guessed$all <- all_cols
  
  return(col_class_guessed)
}
