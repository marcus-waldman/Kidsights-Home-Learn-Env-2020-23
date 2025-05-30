get_parents_acs_list<-function(acs){

  child_acs = acs %>% 
    dplyr::filter(sc_age_years<18)
  
  adult_acs = acs %>% 
    dplyr::filter(a1_age >= 18) %>% 
    dplyr::mutate(aid = uid) %>% 
    dplyr::select(-uid) %>% 
    dplyr::select(aid, a1_age, a1_female, a1_edu4, a1_married)
  
  mom = child_acs %>%
    dplyr::filter(MOMLOC>0) %>%
    dplyr::mutate(aid = paste(famid,MOMLOC, sep = "-")) %>% 
    dplyr::select(uid,aid) %>% 
    dplyr::left_join(adult_acs , by = "aid") %>% 
    dplyr::select(-aid) %>% 
    dplyr::mutate(a1_parent = 1)
  
  dad = child_acs %>%
    dplyr::filter(POPLOC>0) %>%
    dplyr::mutate(aid = paste(famid,POPLOC, sep = "-")) %>% 
    dplyr::select(uid,aid) %>% 
    dplyr::left_join(adult_acs , by = "aid") %>% 
    dplyr::select(-aid) %>% 
    dplyr::mutate(a1_parent = 1)
  
  return(list(mom = mom, dad = dad))

}

