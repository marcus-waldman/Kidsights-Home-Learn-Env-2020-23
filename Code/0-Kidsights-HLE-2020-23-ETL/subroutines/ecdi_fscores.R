#

ecdi_fscores<-function(df, idvar = "ResponseId", intermediate_path){
  
  library(tidyverse)
  library(mirt)
  
  print("\n")
  print("Scoring ECDI...")
  
  
  lex_xwalk = readr::read_csv(file = paste0(intermediate_path,"/items_lex_crosswalk.csv"), show_col_types = F)
  ecdi_items_from = lex_xwalk$lex_ne22[!is.na(lex_xwalk$lex_ecdi)]
  ecdi_items_to = lex_xwalk$lex_ecdi[!is.na(lex_xwalk$lex_ecdi)]
  
  df = df %>% 
    dplyr::select(all_of(c(idvar, ecdi_items_from))) %>% 
    clean_item_responses(item_names = ecdi_items_from)
  names(df) = plyr::mapvalues(names(df), from = ecdi_items_from, to = ecdi_items_to) 
    
  out_df = df %>% 
    dplyr::mutate(ecdi = mirt::mirt(df %>% dplyr::select(all_of(ecdi_items_to))) %>% 
                    mirt::fscores(method = "EAP") %>% 
                    as.vector()) %>% 
    dplyr::select(all_of(idvar), "ecdi")

  return(out_df)
  
}
