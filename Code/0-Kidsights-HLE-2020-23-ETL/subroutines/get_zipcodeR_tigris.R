get_zipcodeR_tigris<-function(state = "NE", yr = 2020, cores = min(c(8,parallel::detectCores()))){
  
  
  ## Load packages
  require(tidyverse)
  require(zipcodeR)
  require(tigris)
  require(sf)
  require(stringr)
  require(pbapply)
  require(ipumsr)
  require(parallel)
  
  cl = parallel::makeCluster(cores)
  print(cl)

  # Get a data frame with all Nebraska ZipCodes
  df_zipcodeR = zipcodeR::search_state(state) %>% dplyr::mutate(tigris_puma = "", tigris_puma.name = "")
  
  ##### Match each zipcode to a PUMA from tigris
        # Download the shape files and match
        df_tigris_pumas_shape = tigris::pumas(state = state, year = yr)
        print("Matching zipcodes to PUMAs")
        zdx = which(!is.na(df_zipcodeR$lat) & !is.na(df_zipcodeR$lng))  
        clusterExport(cl = cl, varlist = c("df_tigris_pumas_shape","zdx","df_zipcodeR"), envir = environment())
      tmp_list <- pblapply(zdx, function(z){
        require(tidyverse)
        require(sf)
        df_zipcodeR_z = df_zipcodeR[z, ]
        
        
        zip_z = sf::st_as_sf(df_zipcodeR_z[,c("lng","lat")], 
                             coords = c("lng","lat"),
                             crs = 4326)  %>% 
          sf::st_transform(26910)
        
        # cycle through each tigris_puma in the sampe file and see if the zip code is contained in that polygon
        for(x in 1:nrow(df_tigris_pumas_shape)){
          
          
          poly_x= df_tigris_pumas_shape$geometry[x] %>% 
            sf::st_transform(26910)
          
          if(nrow(zip_z[poly_x,  ,op=st_within]) == 1){
            df_zipcodeR_z$tigris_puma = substring(df_tigris_pumas_shape$PUMACE10[x],3) %>% as.integer
            df_zipcodeR_z$tigris_puma.name = df_tigris_pumas_shape$NAMELSAD10[x]
            
            break
          }
        }
        return(df_zipcodeR_z)
      }, cl = cl
      ) 
      # Deal with lingering variable type issues with PUMA due to missingness
      for(z in 1:length(tmp_list)){
        if(tmp_list[[z]]$tigris_puma == ""){
          tmp_list[[z]]$tigris_puma = as.integer(NA)
        }
      }
      df_zipcodeR = bind_rows(tmp_list)
      # # Sanity check with nebraska data
      # zip2puma = readr::read_csv("C:/Users/marcu/Dropbox/UNMC/phase-2/progress-reports/zip-to-puma-geocorr2018.csv") %>%
      #   dplyr::mutate(across(where(is.numeric),as.character)) %>% 
      #   group_by(zip) %>%
      #   dplyr::summarize(puma = sample(puma,1)) %>% 
      #   dplyr::mutate(zipcode = zip) %>% 
      #   left_join(df_zipcodeR %>% dplyr::mutate(zip = zipcode, tigris_puma = puma) %>% dplyr::select(zip,tigris_puma), by = "zip")
      # 
      # with(zip2puma, table(puma, tigris_puma, useNA = "ifany")) 
      # # tigris_puma
      # # puma  100 200 300 400 500 600 701 702 801 802 901 902 903 904 <NA>
      # # 100  69   0   0   4   0   0   0   0   0   0   0   0   0   0    1
      # # 200   0  87   1   0   0   0   0   0   0   0   0   0   0   0    4
      # # 300   0   1  49   2   1   0   0   0   0   0   0   0   0   0    0
      # # 400   0   0   2  62   3   0   0   0   0   0   0   0   0   0    1
      # # 500   0   0   2   0  56   1   0   0   0   0   0   0   0   0    1
      # # 600   0   0   0   0   1 108   2   0   1   3   0   0   0   0    4
      # # 701   0   0   0   0   0   0  39   1   1   0   1   0   0   0    1
      # # 702   0   0   0   0   0   0   0  11   0   0   0   0   0   0    0
      # # 801   0   0   0   0   0   0   1   0  17   2   0   0   0   0    0
      # # 802   0   0   0   0   0   1   0   0   1  12   0   0   0   0    0
      # # 901   0   0   0   0   0   0   0   0   0   0   7   0   0   0    0
      # # 902   0   0   0   0   0   0   0   0   0   0   1   6   0   0    0
      # # 903   0   0   0   0   0   0   0   0   0   0   1   0   5   1    2
      # # 904   0   0   0   0   0   0   0   1   0   0   0   2   1   5    0
      # ## Looks reasonable
      # 
  
  
  
  #####
  # Other tigris information can be pulled
      df_zipcodeR = df_zipcodeR %>% dplyr::mutate(tigris_cbsa = FALSE, tigris_cbsa.name = "")
      
      df_tigris_urban_shape = tigris::core_based_statistical_areas(year = yr)
      jds = (df_tigris_urban_shape$NAME %>% str_split_fixed(",",2))[,2] %>% stringr::str_detect("NE") %>% which()
      df_tigris_urban_shape = df_tigris_urban_shape[jds,]
      
      
      print("Matching zipcodes to urban areas")
      zdx = which(!is.na(df_zipcodeR$lat) & !is.na(df_zipcodeR$lng))  
      clusterExport(cl = cl, varlist = c("df_tigris_urban_shape","zdx", "jds","df_zipcodeR"), envir = environment())
      tmp_list <- pblapply(zdx, function(z){
        
        require(tidyverse)
        require(sf)
        
        df_zipcodeR_z = df_zipcodeR[z, ]
        
        
        zip_z = sf::st_as_sf(df_zipcodeR_z[,c("lng","lat")], 
                             coords = c("lng","lat"),
                             crs = 4326)  %>% 
          sf::st_transform(26910)
        
        # cycle through each tigris_puma in the sampe file and see if the zip code is contained in that polygon
        for(j in 1:nrow(df_tigris_urban_shape)){
          
          
          poly_j= df_tigris_urban_shape$geometry[j] %>% 
            sf::st_transform(26910)
          
          if(nrow(zip_z[poly_j,  ,op=st_within]) == 1){
            df_zipcodeR_z$tigris_cbsa = TRUE
            df_zipcodeR_z$tigris_cbsa.name = df_tigris_urban_shape$NAME[j]
            
            break
          }
        }
        return(df_zipcodeR_z)
      }, cl = cl
      ) 
      

      df_zipcodeR = bind_rows(tmp_list)
      
          
  #Return the result
  parallel::stopCluster(cl)
  return(data.frame(df_zipcodeR))
  
}

 

