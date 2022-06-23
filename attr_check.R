attr_check <- function(d_new, d_old){
  attr_names <- list(old = names(attributes(d_old)),
                     new = names(attributes(d_new)))
  attr_checklist <- list()
  attr_checklist$cols_match <-  length(setdiff(colnames(d_old), colnames(d_new))) == 0 & ncol(d_old) == ncol(d_new)
  attr_checklist$attrnames_match <- length(setdiff(attr_names$old, attr_names$new)) == 0 &
    length(attr_names$old) == length(attr_names$new)
  ###testing
  attr_checklist <- as.data.frame(attr_checklist)
  return(attr_checklist)
}