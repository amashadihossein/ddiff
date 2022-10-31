report <- function(diffable_obj){
  
  records_matched <- get(diffable_obj,element = "records_matched")
  var_types <- intersect(c("continuous", "categorical",  "binary","date"), 
            names(records_matched))

  
  # change_ref <- c("modified_from","removed")
  # change_new <- c("modified_to","added")
  
  diff_tally <- sapply(var_types, FUN = function(var_types_i){
    records_matched$meta_records %>% 
      dplyr::select(rowsig_index, record_sig, in_df_ref, in_df) %>%
      dplyr::right_join(x = ., y = records_matched[[var_types_i]]) %>%
      
      dplyr::group_by(variable) %>%
      dplyr::summarise(n_matching = sum(match_status == "matching" & in_df_ref == "Y"),
                       n_modified_from = sum(match_status == "modified_from"),
                       n_modified_to = sum(match_status == "modified_to"),
                       n_added = sum(match_status == "added"),
                       n_removed = sum(match_status == "removed")) %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(n_superset = n_matching + n_modified_from + n_modified_to +
                      n_added + n_removed) %>%
      dplyr::mutate(n_ref = n_matching + n_modified_from + n_removed) %>%
      dplyr::mutate(n_new = n_matching + n_modified_to + n_added) %>%
      dplyr::mutate(n_change_ref = n_modified_from + n_removed) %>%
      dplyr::mutate(n_change_new = n_modified_to + n_added) %>% 
      tidyr::pivot_longer(cols = c(n_matching, n_modified_from, n_modified_to, 
                                   n_added, n_removed, n_change_ref, n_change_new), 
                          values_to = "n", names_prefix = "n_") %>% 
      dplyr::mutate(freq_superset = n/n_superset) %>% 
      dplyr::mutate(freq_ref = n/n_ref) %>% 
      dplyr::mutate(freq_new = n/n_new)
  }, USE.NAMES = T, simplify = F) %>%
    dplyr::bind_rows(.id = "var_type") 
  
  # matching_levels <- c("matching","modified_from","modified_to","added",
  #                      "removed")
  # z %>% tidyr::pivot_longer(cols = -variable,
  #                           names_to = "matching_status",
  #                           values_to = "freq") %>% 
  #   dplyr::mutate(matching_status = factor(matching_status,
  #                                          levels = rev(matching_levels)))) %>%
  #   ggplot(aes(x = variable, y = freq, fill = matching_status)) + geom_col() + 
  # coord_flip()
  
}
