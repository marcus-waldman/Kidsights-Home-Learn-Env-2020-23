wide2long_screener<-function(screener_wide){
  require(tidyverse)
  
  screener_long = 
    lapply(1:5, function(i){
      vars_i = get_child_vars(i)
      hi = screener_wide %>% 
        dplyr::select(all_of(vars_i)) %>% 
        rename_for_long() %>% 
        dplyr::mutate(uid = paste(SQINTRO,ResponseId,i, sep = "-")) %>% 
        dplyr::relocate("uid")
      return(hi)
    }) %>% 
    bind_rows() %>% 
    arrange(uid) %>% 
    dplyr::filter(!is.na(SQCBD_1), !is.na(SQCBD_2), !is.na(SQCBD_3))
  
  
  screener_long = screener_long %>% 
    bind_rows(screener_wide %>% 
                dplyr::filter(!(ResponseId %in% screener_long$ResponseId)) %>% 
                dplyr::select(all_of(get_child_vars(1))) %>% 
                rename_for_long()
    )
  
  return(screener_long)
  
}


