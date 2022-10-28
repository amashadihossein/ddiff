#' @title Match records across two data.frames. 
#' @description This is an internal function called only from within `diffable` 
#' when constructing a diffable object
#' @param diffable_obj diffable_obj
#' @param mincol_match_rate a value between 0 and 1 to determine min rate of 
#' match to qualify as modification (vs new record)
#' @return a data.frame of metadata on the set of all records (i.e. both df and
#' df_ref) and the status of matching at row (record level) as well as well each
#' element within the matrix. The values could be "matching", "removed", 
#' "added", "modified_from" and "modified_to"
#' @example 
#' old <- mtcars[1:6,]
#' new <- mtcars[3:8,]
#' new <- new[rev(1:nrow(new)),]
#' old$carb[4]<-NA
#' old$cyl[6]<-12
#' diffable_obj <- diffable(df = new,df_ref = old)
#' get(diffable_obj, "records_matched")
#' @keywords internal


#TODO: remove rows that are just NAs
match_record <- function(diffable_obj, mincol_match_rate = 0.5){

  # browser()
  
  df <- get(diffable_obj, element = "df", include_metadata_cols = T)
  df_ref <- get(diffable_obj, element = "df_ref", include_metadata_cols = T)
  
  ncol_match_th <- mincol_match_rate * ncol(df)
  record_sigs <- list(matching = intersect(df_ref$record_sig, df$record_sig))
  record_sigs$unmatch <- setdiff(df$record_sig, record_sigs$matching)
  record_sigs$unmatch_ref <- setdiff(df_ref$record_sig, record_sigs$matching)

  record_sigs$modified <- 
    data.frame(matrix(nrow=0, ncol=3)) %>% `colnames<-`(c("df_ref_rowsig",
                                                          "df_rowsig", 
                                                          "modify_match")) 
  
  #TODO take out sig from complex match
  if(length(record_sigs$unmatch)>0 & length(record_sigs$unmatch_re)>0){

    df_unmatched <- df[df$record_sig %in% record_sigs$unmatch,,drop=F]
    df_ref_unmatched <- df_ref[df_ref$record_sig %in% record_sigs$unmatch_ref,,drop=F]
    
    df_unmatched_ct <- data.frame(t(sapply(df_unmatched, as.character)))
    df_ref_unmatched_ct <- data.frame(t(sapply(df_ref_unmatched, as.character)))
    
    colnames(df_unmatched_ct) <- df_unmatched$record_sig
    colnames(df_ref_unmatched_ct) <- df_ref_unmatched$record_sig
    df_unmatched_ct <- df_unmatched_ct[setdiff(rownames(df_unmatched_ct),"record_sig"),,drop=F]
    df_ref_unmatched_ct <- df_ref_unmatched_ct[setdiff(rownames(df_ref_unmatched_ct),"record_sig"),,drop=F]
    
    
    row_match_tally <- sapply(df_unmatched_ct, function(x) 
      colSums(x == df_ref_unmatched_ct, na.rm = T),USE.NAMES = T) %>%
      data.frame(.,check.names = F) %>%
      dplyr::mutate("df_ref_rowsig" = rownames(.)) %>% 
      tidyr::gather(key = "df_rowsig",value = "match_tally",-c("df_ref_rowsig"))
    
    
    record_sigs$modified <- row_match_tally %>% dplyr::group_by(df_rowsig) %>%
      dplyr::mutate(modify_match = match_tally == max(match_tally) &
                      match_tally > ncol_match_th) %>% dplyr::ungroup() %>% 
      dplyr::distinct(modify_match, df_rowsig, .keep_all = T) %>% 
      dplyr::filter(modify_match == T) %>%
      dplyr::select(df_ref_rowsig , df_rowsig)

  }
  
  all_sigs <- union(df_ref$record_sig, df$record_sig)
  record_sigs$all <- data.frame(rowsig = all_sigs) %>%
    dplyr::mutate(in_df_ref = dplyr::if_else(rowsig %in% df_ref$record_sig, "Y","N")) %>%
    dplyr::mutate(in_df = dplyr::if_else(rowsig %in% df$record_sig, "Y","N")) %>%
    dplyr::mutate(maps_to = replace(rowsig, in_df_ref == "N" | in_df == "N", NA)) %>% 
    dplyr::left_join(x = ., y = record_sigs$modified %>%
                       dplyr::rename(rowsig = df_ref_rowsig)) %>%
    dplyr::left_join(x = ., y = record_sigs$modified %>% 
                       dplyr::rename(rowsig = df_rowsig)) %>%
    dplyr::mutate(maps_to = dplyr::coalesce(maps_to,df_ref_rowsig,df_rowsig)) %>%
    dplyr::select(rowsig, in_df_ref,in_df,maps_to) %>%
    dplyr::mutate(record_match_status = 
                    dplyr::case_when(rowsig == maps_to ~ "matching",
                                     rowsig != maps_to &
                                       in_df_ref == "Y" & in_df == "N" ~ "modified_from",
                                     rowsig != maps_to &
                                       in_df_ref == "N" & in_df == "Y" ~ "modified_to",
                                     in_df_ref == "Y" & in_df == "N" ~ "removed",
                                     in_df_ref == "N" & in_df == "Y" ~ "added",
                                     TRUE ~ "unknown")) %>% 
    dplyr::mutate(rowsig_index = match(maps_to,rowsig)) %>% 
    dplyr::mutate(rowsig_index=replace(rowsig_index,record_match_status!= "modified_to",NA)+.5)
  
  records_modified_to <- record_sigs$all %>% 
    dplyr::filter(!is.na(rowsig_index)) %>% 
    dplyr::select(rowsig_index, rowsig) 
  
  records_but_modified_to <- record_sigs$all %>% 
    dplyr::filter(record_match_status != "modified_to") %>%
    dplyr::mutate(rowsig_index = 1:nrow(.)) %>%
    dplyr::select(rowsig_index, rowsig)
    
  record_sigs$all <- dplyr::bind_rows(records_but_modified_to, records_modified_to) %>%
    dplyr::left_join(x = .,  record_sigs$all %>% dplyr::select(-rowsig_index)) %>% 
    dplyr::arrange(rowsig_index) %>%
    dplyr::rename(record_sig = rowsig)
  
  records_matched <- dplyr::bind_rows(df_ref,df, df_ref) %>%
    dplyr::distinct() %>%
    dplyr::left_join(record_sigs$all, .)
  
  
  #--
  # browser()

  index_matched <- to_index_matchstatus(records_matched = records_matched,
                                         obj_colnames = get(diffable_obj, "col_names"))
  
  d_restruct<- structure_matched_records(records_matched = , records_matched,
                                  col_types = get(diffable_obj, "col_types"),
                                  col_names = get(diffable_obj, "col_names"),
                                  index_matched = index_matched)
      
  invisible(d_restruct)
}


#' @description this is an internal function only used to go from matching at 
#' the record level to matching at cell level
#' @title Convert to index (i.e. matrix element) level matching
#' @param records_matched data.frame with records matching metadata
#' @param obj_colnames shared colnames across the two datasets
#' @return a data.frame of metadata on the set of all records (i.e. both df and
#' df_ref) and the status of matching at row (record level) as well as well each
#' element within the matrix. The values could be "matching", "removed", 
#' "added", "modified_from" and "modified_to"
#' @keywords internal
to_index_matchstatus <- function(records_matched, obj_colnames){
  
  ndistinct_colnames <- paste0("ndistinct_",obj_colnames)
  
  tmp <- records_matched %>% 
    dplyr::filter(record_match_status == "modified_from") %>%
    dplyr::select(record_sig, maps_to) %>% 
    dplyr::mutate(pair_id = as.character(1:nrow(.))) %>%
    tidyr::pivot_longer(cols = c(record_sig, maps_to), values_to = "record_sig") %>%
    dplyr::select(record_sig, pair_id) %>%
    dplyr::left_join(records_matched,.) %>%
    dplyr::relocate(rowsig_index, pair_id)
  
  match_index_status <- tmp %>% 
    dplyr::select(c("pair_id",obj_colnames)) %>% 
    tidyr::drop_na(pair_id) %>%
    dplyr::group_by(pair_id) %>% 
    dplyr:: summarise(across(obj_colnames, dplyr::n_distinct)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = obj_colnames) %>%
    tidyr::pivot_wider(names_from = name, names_prefix = "ndistinct_", 
                       values_from = value) %>%
    dplyr::left_join(tmp,.) %>%
    dplyr::select(c("record_sig","record_match_status", ndistinct_colnames)) %>%
    
    tidyr::pivot_longer(cols = ndistinct_colnames, values_to = "n_distinct") %>%
    
    dplyr::mutate(name = gsub("ndistinct_", replacement = "", x = name)) %>%
    dplyr::rename(names_from = name) %>% 
    dplyr::mutate(match_index = 
                    dplyr::case_when(
                      record_match_status == "modified_from" & n_distinct == 1  ~ "matching",
                      record_match_status == "modified_to" & n_distinct == 1  ~ "matching",
                      TRUE ~ record_match_status)) %>% 
    dplyr::select(-c(n_distinct)) %>%
    tidyr::pivot_wider(names_from = names_from, values_from = match_index)
  
  match_index_status <- tmp %>% dplyr::select(!dplyr::any_of(obj_colnames)) %>%
    dplyr::left_join(x = ., match_index_status)
  
  return(match_index_status)
}


structure_matched_records <- function(records_matched,col_types, col_names,
                                      index_matched){
  
  d_all_records <- list()
  if(length(col_types$continuous)>0){
    
    d_all_records$continuous <- records_matched %>% 
      dplyr::select("record_sig",col_types$continuous) %>%
      tidyr::pivot_longer(cols = - record_sig, names_to = "variable")
    
    d_all_records$continuous <- index_matched %>%
      dplyr::select("record_sig",col_types$continuous) %>%
      tidyr::pivot_longer(cols = - record_sig, values_to = "match_status",
                          names_to = "variable") %>%
      dplyr::full_join(x = ., y = d_all_records$continuous )
    
  }
    
  
  if(length(col_types$categorical)>0){
    d_all_records$categorical <- records_matched %>% 
      dplyr::select("record_sig",col_types$categorical) %>%
      dplyr::mutate(dplyr::across(col_types$categorical, ~ format(.x, trim = T))) %>%
      tidyr::pivot_longer(cols = - record_sig, names_to = "variable")
    
    d_all_records$categorical <- index_matched %>%
      dplyr::select("record_sig",col_types$categorical) %>%
      tidyr::pivot_longer(cols = - record_sig, values_to = "match_status",
                          names_to = "variable") %>%
      dplyr::full_join(x = ., y = d_all_records$categorical )
  }

  
  if(length(col_types$binary)>0){
    d_all_records$binary <- records_matched %>% 
      dplyr::select("record_sig",col_types$binary) %>%
      dplyr::mutate(dplyr::across(col_types$binary, ~ format(.x, trim = T))) %>%
      tidyr::pivot_longer(cols = - record_sig, names_to = "variable")
    
    d_all_records$binary <- index_matched %>%
      dplyr::select("record_sig",col_types$binary) %>%
      tidyr::pivot_longer(cols = - record_sig, values_to = "match_status",
                          names_to = "variable") %>%
      dplyr::full_join(x = ., y = d_all_records$binary )
    
  }

  
  if(length(col_types$date)>0){
    d_all_records$date <- records_matched %>% 
      dplyr::select("record_sig",col_types$date) %>%
      tidyr::pivot_longer(cols = - record_sig, names_to = "variable")
    
    d_all_records$date <- index_matched %>%
      dplyr::select("record_sig",col_types$date) %>%
      tidyr::pivot_longer(cols = - record_sig, values_to = "match_status",
                          names_to = "variable") %>%
      dplyr::full_join(x = ., y = d_all_records$date )
  }
  
  d_all_records$meta_records <-
    records_matched %>% dplyr::select(!dplyr::one_of(col_names))
  
  return(d_all_records)
}
