
find_multi_inconsis_ageform_responses<-function(df){
  
  require(tidyverse)
  
  vars = get_ageform_vars(df)
  
  out_df = data.frame(ResponseId = df$ResponseId, multi_inconsis = 0, variable = "")
  
  
  for(j in 1:length(vars)){
    
    df_j = df %>% dplyr::select(starts_with(vars[j]))
    nunique_vec = df_j %>% 
      apply(1, function(x){length(unique(x[!is.na(x)]))}) 
    out_df$multi_inconsis[nunique_vec>1] = 1
    out_df$variable[nunique_vec>1] = paste(out_df$variable[nunique_vec>1], vars[j], sep = " ") 
    
  }
  
  return(out_df)
  
}


  
  

