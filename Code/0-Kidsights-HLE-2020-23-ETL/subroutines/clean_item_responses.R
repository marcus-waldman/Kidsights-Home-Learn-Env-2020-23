clean_item_responses <- function(dirty,item_names=names(dirty)[-1]){
  
  require(tidyverse)
  
  dirty = dirty %>% dplyr::mutate(across(where(is.ordered), function(x){as.integer(x)-1}))
  dirty = zap_attributes(dirty)
  
  # Exclude items with no variability
  item_sd = dirty %>% 
    dplyr::select(all_of(item_names)) %>% 
    apply(2,sd,na.rm = T)
  items_discard = item_names[item_sd<.01]
  dirty = dirty %>% dplyr::select(-all_of(items_discard))
  item_names = item_names[item_sd>=.01]
  
  #Ensure that there are incremental responses and no jumps
  dirty %>% dplyr::select(all_of(item_names)) %>% apply(2,maxseqdiff)  # lookds good
  items_skipping = item_names[(dirty %>% dplyr::select(all_of(item_names)) %>% apply(2,maxseqdiff)) > 1]
  if(length(items_skipping)>1){
    for(j in 1:length(items_skipping)){
      jvar = items_skipping[1]
      y_from = unique(dirty[,jvar]) %>% sort()
      y_to = seq(0,length(y_from)-1, by = 1)
      dirty[,jvar] = plyr::mapvalues(dirty[,jvar],from = y_from, to = y_to)
    }
  }
  
  #Exclude those with all item responses missing
  prop_miss = dirty %>% 
    dplyr::select(all_of(item_names)) %>% 
    apply(1,function(x)mean(is.na(x)))
  dirty = dirty[prop_miss<1, ]
  
  
  clean = dirty
  return(clean)
  
  
}