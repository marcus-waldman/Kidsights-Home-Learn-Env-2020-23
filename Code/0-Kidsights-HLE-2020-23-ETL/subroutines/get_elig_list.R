get_elig_list<-function(SCREENER_filename, SURVEY_filename, path = "C:/Users/marcu/Dropbox/UNMC/phase-2", onedrive_root = "C:/Users/marcu", reference_df = NULL){
  
  if(is.null(reference_df)){
    reference_df = readr::read_rds(file = paste0(path, "/data-files/intermediate-files/cahmi_2016_2020.RDS")) %>% 
      dplyr::filter(fipsst == 31) %>%  
      recode_cahmi(path = path, cahmi_2016_2020 = .)
  }
  
  # Load in zipcodeR_tigris
  zipcodeR_tigris = readRDS(paste0(path, "/data-files/intermediate-files/zipcodeR_tigris.RDS"))
  
  # Load in the KH acceptable zip codes
  zipcodes_df = get_KH_acceptable_zipcodes(onedrive_root)
  
  # Load in the latest SCREENER pull
  SCREENER = haven::read_sav(file = paste0(path,"/data-files/source-files/Qualtrics/",SCREENER_filename))
  screener_df = SCREENER
  
  # Load in the latest SURVEY pull
  SURVEY = haven::read_sav(file = paste0(path,"/data-files/source-files/Qualtrics/", SURVEY_filename))
  survey_df = SURVEY %>% 
    recode_qualtrics_survey(reference_df = reference_df, zipcodeR_tigris = zipcodeR_tigris) %>% 
    recode_items()
  
  # Load in IP Hub
  IP_Hub = readRDS(paste0(path,"/data-files/intermediate-files/IP_Hub.rds")) %>% na.omit()
  IP_Hub = update_IP_Hub(IPs = c(screener_df %>% dplyr::filter(IPAddress != "") %>% purrr::pluck("IPAddress"), 
                                 survey_df %>% dplyr::filter(IPAddress != "") %>% purrr::pluck("IPAddress")) %>% 
                           unique(), 
                         IP_Hub = IP_Hub)
  
  
  elig_list = evaluate_eligibility(screener_df, survey_df, ref_df = reference_df, IP_Hub, zipcodeR_tigris, zipcodes_df)
  
  return(elig_list)
  
}



