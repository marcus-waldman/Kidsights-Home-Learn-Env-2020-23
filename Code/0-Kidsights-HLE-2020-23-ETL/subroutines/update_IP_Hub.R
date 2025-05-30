#IPs = unique(c(screener_df$IPAddress, survey_df$IPAddress))

update_IP_Hub<-function(IPs, IP_Hub, 
                        key_ip_hub = "MTk2NDY6THNVVjJjZmVoV0lpeWJ3emJ1b3pZMHozdzlWRUxVSmw=", #"MTg3MzM6RUxiUUVQeFlGZkxRWnRUdlZINEtQTkFnYnR3WFJxR2c=", 
                        root_wd = "C:/Users/marcu", 
                        save_update = T){
  

  require(tidyverse)
  require(rIP)
  
  IP_Hub_old = IP_Hub;
  
  if(length(setdiff(IPs,IP_Hub_old$IPAddress))>0){
    
    IP_Hub_to_add = data.frame(IPAddress = setdiff(IPs,IP_Hub_old$IPAddress)) %>% 
      dplyr::filter(IPAddress != "")  %>% 
      rIP::getIPinfo("IPAddress", key_ip_hub)
    
    ip2_bin_path = paste0(root_wd,"/Dropbox/UNMC/phase-2/data-files/source-files/ip2location/IP-COUNTRY-REGION-CITY-LATITUDE-LONGITUDE-ZIPCODE.BIN")
    ip2location::open(ip2_bin_path)
    tmp = pblapply(IP_Hub_to_add$IPAddress, function(ip){
      ip2location::get_all(ip) %>% bind_cols
    }) %>% bind_rows
    names(tmp) = paste0("ip2_", names(tmp))
    names(tmp)[names(tmp)=="ip2_ip"] = "IPAddress"
    
    IP_Hub_to_add = IP_Hub_to_add %>% dplyr::left_join(tmp,by = "IPAddress")
    
    IP_Hub = bind_rows(IP_Hub_old, IP_Hub_to_add)
    
    if(save_update){
      require(stringr)
      time_stamp = Sys.time() %>% as.character() %>% str_replace_all(":","_")
      saveRDS(IP_Hub_old, file = paste0(root_wd, "/Dropbox/UNMC/phase-2/data-files/intermediate-files/archieve/IP_Hub",time_stamp,".RDS"))
      saveRDS(IP_Hub, file = paste0(root_wd, "/Dropbox/UNMC/phase-2/data-files/intermediate-files/IP_Hub.RDS"))
    }
    
  } else{
    
    print("IP_Hub is already up-to-date :-)")

  }

  return(IP_Hub)
}