recode_items<-function(df, intermed_path){
  require(tidyverse)
  df = combine_ageform_items(df)
  
  items_lex_crosswalk = readr::read_csv(paste0(intermed_path,"/items_lex_crosswalk.csv"))
  
  vl_all <- lapply(1:nrow(items_lex_crosswalk), function(j){
    vals_j = sjlabelled::get_labels(df %>% purrr::pluck(items_lex_crosswalk$lex_ne22[j]), values = "n") %>% names() %>%  as.numeric() 
    labels_j = sjlabelled::get_labels(df %>% purrr::pluck(items_lex_crosswalk$lex_ne22[j]))
    vl_j = data.frame(labels = labels_j, values = vals_j)
    return(vl_j)
    
  }) 
  
  vl_unique = unique(vl_all)
  
  #vl_unique_check = vl_unique
  #saveRDS(vl_unique_check, file =  "C:/Users/marcu/Dropbox/UNMC/phase-2/data-files/intermediate-files/item_value_label_mapping_list.RDS")
  vl_unique_check = readRDS(file = paste0(intermed_path, "/item_value_label_mapping_list.RDS"))
  
  if(!identical(vl_unique, vl_unique_check)){
    stop("Need to update item_value_label_mapping_list.RDS as current matchings do not match")
  }
  
  
  items_lex_crosswalk$pattern = NA
  for(j in 1:nrow(items_lex_crosswalk)){
    
    for(k in 1:length(vl_unique)){
      if(identical(vl_all[[j]],vl_unique[[k]])){
        items_lex_crosswalk$pattern[j] = k
        break
      }
    }
    
  } 
  #readr::write_rds(items_lex_crosswalk, file = "C:/Users/marcu/Dropbox/UNMC/ECD Rapid/Data/items_lex_crosswalk.RDS")
  
  
  
  # [[1]]
  # labels values
  # 1        Yes      1
  # 2         No      2
  # 3 Don't Know      3
  vl_unique[[1]]$values_to = c(1,0,NA)
  
  # 
  # [[2]]
  #                 labels values
  # 1 Not at all confident      1
  # 2   Somewhat confident      2
  # 3       Very confident      3
  vl_unique[[2]]$values_to = c(0,1,2)
  
  # 
  # [[3]]
  #       labels values
  # 1        Yes      1
  # 2         No      2
  # 3 Don't know      3
  vl_unique[[3]]$values_to = c(1,0,NA)
  
  
  # [[4]]
  # labels values
  # 1    Yes      1
  # 2     No      2
  vl_unique[[4]]$values_to = c(1,0)
  
  # 
  # [[5]]
  # labels values
  # 1 Not at all      1
  # 2       Less      2
  # 3   The same      4
  # 4       More      5
  # 5 A lot more      6
  vl_unique[[5]]$values_to = c(0,1,2,3,4)
  
  
  # 
  # [[6]]
  # labels values
  # 1      Not true      1
  # 2 Somewhat true      2
  # 3     Very true      3
  vl_unique[[6]]$values_to = c(0,1,2)
  
  
  # [[7]]
  # labels values
  # 1 None of the time      1
  # 2 Some of the time      2
  # 3 Half of the time      3
  # 4 Most of the time      4
  # 5  All of the time      5
  vl_unique[[7]]$values_to = c(0:4)
  
  
  # 
  # [[8]]
  # labels values
  # 1    Child cannot count      1
  # 2  Child can count to 5      2
  # 3 Child can count to 10      3
  # 4 Child can count to 15      4
  # 5 Child can count to 20      5
  # 6 Child can count to 30      6
  vl_unique[[8]]$values_to = c(0:5)
  
  
  # 
  # [[9]]
  # labels values
  # 1 None of the time      1
  # 2 Some of the time      2
  # 3 Most of the time      3
  # 4  All of the time      4
  # 5 Half of the time      5
  vl_unique[[9]]$values_to = c(0:4)
  
  
  # [[10]]
  # labels values
  # 1 None of them      1
  # 2 Some of them      2
  # 3 Most of them      3
  # 4  All of them      4
  vl_unique[[10]]$values_to = c(0:3)
  
  
  # 
  # [[11]]
  # labels values
  # 1      Poor      1
  # 2      Fair      2
  # 3      Good      3
  # 4 Very good      4
  # 5 Excellent      5
  vl_unique[[11]]$values_to = c(0:4)
  
  
  # 
  # [[12]]
  # labels values
  # 1    Daily activities consistently affected/Often a great deal      1
  # 2        Daily activities moderately affected some of the time      3
  # 3 Does not have any conditions/Daily activities never affected      4
  vl_unique[[12]]$values_to = c(0:2)
  
  
  # 
  # [[13]]
  # labels values
  # 1 A lot of difficulty      1
  # 2 A little difficulty      2
  # 3       No difficulty      3
  vl_unique[[13]]$values_to = c(0:2)
  
  
  
  
  # [[14]]
  # labels values
  # 1      Often      1
  # 2  Sometimes      2
  # 3      Never      3
  # 4 Don't know      4
  vl_unique[[14]]$values_to = c(2,1,0,NA)
  
  
  
  # [[15]]
  #                  labels values
  # 1                 Often      1
  # 2             Sometimes      2
  # 3 Never or Almost Never      3
  # 4            Don't Know      4
  vl_unique[[15]]$values_to = c(2,1,0,NA)
  
  
  tmp <-pblapply(1:nrow(items_lex_crosswalk), function(j){
    
    k_j = items_lex_crosswalk$pattern[j]
    label_j = df %>% purrr::pluck(items_lex_crosswalk$lex_ne22[j]) %>% sjlabelled::get_label()
    y_j = df %>%
      purrr::pluck(items_lex_crosswalk$lex_ne22[j]) %>% 
      zap_attributes() %>% 
      as.numeric() %>% 
      plyr::mapvalues(from = vl_unique[[k_j]]$values, 
                      to = vl_unique[[k_j]]$values_to, 
                      warn_missing = F)
    
    df_j = data.frame(y = y_j)
    labels_j = vl_unique[[k_j]]$values_to
    names(labels_j) = vl_unique[[k_j]]$labels
    df_j$y = haven::labelled(x = df_j$y, labels = labels_j, label = label_j)
    names(df_j) = items_lex_crosswalk$lex_ne22[j]
    
    
    return(df_j)
    
  }) %>% bind_cols() %>% 
    dplyr::mutate(ResponseId = df$ResponseId)

  df = df %>% 
    dplyr::select(-all_of(items_lex_crosswalk$lex_ne22)) %>% 
    dplyr::left_join(tmp, by = "ResponseId")
  
  return(df)
  
  
}


