get_ageform_vars<-function(df){
  
  require(tidyverse)
  require(stringr)
  require(purrr)
  
  tmp = names(df)[(df %>% names() %>% stringr::str_detect(pattern = "\\.[1-9]"))] %>% 
    stringr::str_split_fixed(pattern = "\\.[1-9]", n=2) %>% 
    data.frame()
  names(tmp) = c("prefix", "suffix")
  
  
  vars = tmp %>% 
    dplyr::mutate(suffix = as.integer(suffix)) %>%  
    dplyr::filter(suffix <= 9 | is.na(suffix) ) %>% 
    dplyr::filter(!startsWith(prefix,"FCI")) %>% 
    purrr::pluck("prefix") %>% 
    unique() %>% 
    sort()
  
  tmp = names(df)[startsWith(names(df), "NOM") & (endsWith(names(df),"A") | endsWith(names(df),"B"))] %>%  substr(.,1,6) %>% unique() %>% sort()
  vars = c(vars,tmp)
  
  vars = vars[-which(startsWith(vars,"CQR"))]
  

  return(vars)
  
}
