
get_dscores<-function(input_df, intermediate_path = "C:/Users/marcu/Dropbox/UNMC/phase-2/data-files/intermediate-files", idvar = "ResponseId"){


    lex_xwalk = readr::read_csv(file = paste0(intermediate_path,"/items_lex_crosswalk.csv"), show_col_types = F)
    
    items_lex_ne22 = lex_xwalk$lex_ne22[!is.na(lex_xwalk$lex_gsed2)]
    items_lex_gsed2 = lex_xwalk$lex_gsed2[!is.na(lex_xwalk$lex_gsed2)]
    key_gsed2 = "gsed2208"
    
    
    gsed_df = input_df %>%  
      dplyr::select(all_of(idvar), days, all_of(items_lex_ne22)) %>% 
      dplyr::filter(days < (42/12)*365.25)
    
    names(gsed_df) = plyr::mapvalues(names(gsed_df), 
                                      from = c("days",idvar,items_lex_ne22), 
                                      to = c("days",idvar,items_lex_gsed2))
    
    print("Scoring to get d-scores...")
    out_dscore = dscore::dscore(data = gsed_df, items = items_lex_gsed2, xname = "days", xunit = "days", key = "gsed2208")
    
    out_df = data.frame(id = gsed_df[,idvar], dscore = out_dscore$d, daz_score = out_dscore$daz, dscore_sem = out_dscore$sem)
    out_df = out_df %>% dplyr::rename_with(function(x)idvar, .cols = "id")
    
    return(out_df)
}