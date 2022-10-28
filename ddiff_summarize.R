ddiff_summarize <- function(diffable_obj){
  
  records_matched <- get(diffable_obj,element = "records_matched")
  var_types <- intersect(c("continuous", "categorical",  "binary","date"), 
            names(records_matched))
  out_template <-data.frame(variable="none", match_status="unknown", freq = 0)
  
  matching_levels <- c("matching","modified_from","modified_to","added",
                       "removed")
  
  z <- lapply(records_matched[var_types],FUN = function(x){
    x %>% dplyr::group_by(variable) %>%
      dplyr::add_count(name = "N") %>% 
      dplyr::group_by(variable,match_status) %>%
      dplyr::add_count(name = "n_lev") %>%
      dplyr::mutate(freq = n_lev/N) %>%
      dplyr::distinct(variable,match_status,freq) %>%
      dplyr::ungroup() %>%
      data.frame()
    }) %>% dplyr::bind_rows() %>%
    tidyr::pivot_wider(names_from = match_status,values_from = freq) %>%
    dplyr::mutate(dplyr::across(.cols = !dplyr::one_of("variable"),
                                ~ tidyr::replace_na(.x, 0)))
  
  # z %>% tidyr::pivot_longer(cols = -variable,
  #                           names_to = "matching_status",
  #                           values_to = "freq") %>% 
  #   dplyr::mutate(matching_status = factor(matching_status,
  #                                          levels = rev(matching_levels)))) %>%
  #   ggplot(aes(x = variable, y = freq, fill = matching_status)) + geom_col() + 
  # coord_flip()
  
}
