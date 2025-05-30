mutate_contrast_vars<-function(df){
  library(tidyverse)
  out = df %>% dplyr::mutate(
    white = startsWith(race4_char,"White") %>% as.integer(),
    black = startsWith(race4_char,"Black") %>% as.integer(),
    other = startsWith(race4_char,"Other") %>% as.integer(),
    below_fpl185 = ( fpl_i1<185) %>% as.integer(),
    above_fpl185 = ( fpl_i1>=185) %>% as.integer(),
    hs = (a1_edu4==1) %>% as.integer(),
    no_hs = (a1_edu4==0) %>% as.integer(), 
    some_college = (a1_edu4==2) %>% as.integer(),
    bachelors = (a1_edu4==3) %>% as.integer(), 
    no_bachelors = 1-bachelors,
    urban = 1-rural
  )
  
  return(out)
    
}