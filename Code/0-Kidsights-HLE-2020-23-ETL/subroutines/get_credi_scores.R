# Run tmp.r up to line 53
get_credi_scores<-function(input_df, intermediate_path, idvar = "ResponseId"){
  
  lex_xwalk = readr::read_csv(file = paste0(intermediate_path,"/items_lex_crosswalk.csv"), show_col_types = F)
  
  items_lex_ne22 = lex_xwalk$lex_ne22[!is.na(lex_xwalk$lex_credi)]
  items_lex_credi = lex_xwalk$lex_credi[!is.na(lex_xwalk$lex_credi)]
  
  
  id_map = input_df %>% dplyr::mutate(ID = 1:nrow(.)) %>% dplyr::select(all_of(idvar),ID)
  
  credi_df = input_df %>%  
    dplyr::left_join(id_map, by = idvar) %>% 
    dplyr::mutate(month = 12*(days/365.25)) %>% 
    dplyr::select(month,ID,dplyr::any_of(items_lex_ne22)) %>% 
    dplyr::filter(month<=36)
  
  names(credi_df) = plyr::mapvalues(names(credi_df), 
                                    from = c("month","ID",items_lex_ne22), 
                                    to = c("AGE","ID",items_lex_credi))
  
  credi_df = credi_df %>% dplyr::select(-starts_with("LMH"))
  
 
  
  credi_df = credi_df %>% zap_attributes()
  
  print("Getting CREDI scores...")
  out_credi = credi::score(data = credi_df, reverse_code = T, interactive = F)
  
  bar = out_credi$scores %>% 
    dplyr::select(-starts_with("LF")) 
  names(bar) = paste0("credi_",names(bar))
  
  
  bar = bar %>% 
    dplyr::mutate(ID = credi_ID) %>%
    dplyr::left_join(id_map, by = "ID") 
  
  bar = bar %>% 
    dplyr::select(-credi_ID,-credi_AGE, -ID) %>% 
    relocate(starts_with(idvar))
  
  
  return(bar)
}

