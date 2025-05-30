extract_vars_cahmi<-function(source,cahmi_2019_2020, cahmi_2017_2018, cahmi_2016,vars=NULL){
  
  extracted_df = NULL
  if(source == "2021"){
    error("Data for 2021 has not been collected")
  }
  if(source == "2019-2020"){
    extracted_df = cahmi_2019_2020 %>% transform(fwc = fwc_1920) %>%  
      dplyr::select(year,fipsst,stratum,hhid, fwc, all_of(vars)) %>% 
      mutate(fwc = 2*fwc/5) %>% 
      mutate(hhid = as.character(hhid))
  }
  if(source == "2017-2018"){
    extracted_df = cahmi_2017_2018 %>% transform(fwc = FWC_1718) %>% 
      mutate(fwc = 2*fwc/5) %>% 
      dplyr::select(YEAR,FIPSST,STRATUM,HHID, fwc, all_of(vars))
    names(extracted_df) = tolower(names(extracted_df))
    extracted_df = extracted_df %>% mutate(stratum = as.character(stratum), 
                                           hhid = as.character(hhid))
  }
  if (source == "2016"){
    extracted_df= cahmi_2016 %>% 
      dplyr::mutate(fwc = 1*FWC/5) %>% 
      dplyr::select(YEAR,FIPSST,STRATUM,HHID, fwc, all_of(vars))
    names(extracted_df) = tolower(names(extracted_df))
  }
  
  return(extracted_df)
}
