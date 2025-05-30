get_KH_acceptable_zipcodes<-function(onedrive_phase2_data_path){
  
  require(readxl)
  require(zipcodeR)
  
  # Load in the zip code data 
  zipcodes_df = readxl::read_excel(file.path(onedrive_phase2_data_path,"Zip_County_Code_UPDATED10.18.xlsx"), sheet = "Master") %>% 
    dplyr::mutate(County = stringr::str_remove_all(County," County"))
  # Check to see that all Nebraska zipcodes are in KH and/or zipcodeR data set
  zipcodeR::search_state("NE") %>% 
    dplyr::filter(zipcode %in%  setdiff(zipcodeR::search_state("NE") %>% purrr::pluck("zipcode"), unique(zipcodes_df$ZipCode))) %>% 
    dplyr::select(zipcode:major_city, county, radius_in_miles, population) %>% 
    arrange(county) %>% 
    knitr::kable(caption = "Zipcodes in zipcodeR package but not in KH Database") #Some missing zip codes from KH list!
  zipcodes_df %>% 
    dplyr::filter(ZipCode %in% setdiff(unique(zipcodes_df$ZipCode), zipcodeR::search_state("NE") %>% purrr::pluck("zipcode"))) %>% 
    dplyr::arrange(ZipCode) %>% 
    dplyr::group_by(ZipCode) %>% 
    dplyr::summarize(Counties = paste(County, collapse = " + ")) %>% 
    dplyr::arrange(ZipCode) %>% 
    knitr::kable(caption = "Zipcodes in KH database but not in zipcodeR database") #Are there missing zipcodes in zipcodeR database?
  # Check to see if the counties listed by a KH zipcode contains the county from the zipcodeR database
  foo = zipcodes_df %>% 
    dplyr::mutate(ZipCode = as.character(ZipCode)) %>% 
    dplyr::filter(ZipCode %in% (zipcodeR::search_state("NE") %>% purrr::pluck("zipcode"))) %>% 
    dplyr::left_join(zipcodeR::search_state("NE") %>% 
                dplyr::mutate(zipcodeR_cnty = str_remove_all(county," County"), 
                              ZipCode = zipcode) %>% 
                dplyr::select(ZipCode, zipcodeR_cnty), 
              by = "ZipCode") %>% 
    dplyr::group_by(ZipCode) %>% 
    dplyr::summarize(contains_zipcodeR_cnty = (zipcodeR_cnty[1] %in% County), 
              zipcodeR_cnty = zipcodeR_cnty[1], 
              KH_counties = paste(County, collapse = " + ")) %>% 
    dplyr::filter(!contains_zipcodeR_cnty)
  print(paste("There are",nrow(foo),"zipcodes from KH database with a non matching zipcodeR county")) 
  
  
  return(zipcodes_df)
}


