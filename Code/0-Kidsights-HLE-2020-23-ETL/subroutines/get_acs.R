get_acs<-function(path, save_out = T){
  
  require(ipumsr)
  raw_acs <- ipumsr::read_ipums_micro(ddi = ipumsr::read_ipums_ddi(paste0(path,"/data-files/source-files/acs5_2020/usa_00008.xml")))
  
  if(save_out){
    readr::write_rds(raw_acs, file = paste0(path,"/data-files/intermediate-files/acs5_2020.RDS"))
  }
  
  return(raw_acs)
}

   
    
  