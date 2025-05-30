label_it = function(x, var, ref_df){
  require(haven)
  require(purrr)
  require(ipumsr)
  require(tidyverse)
  x = x %>%  
    haven::labelled(label = ref_df %>% purrr::pluck(var) %>%  sjlabelled::get_label())
  
  if(!is.character(x)){
    labs = sjlabelled::get_labels(ref_df %>% purrr::pluck(var), values = "n") %>% names() %>%  as.numeric() 
    if(length(labs)>0){
      names(labs) = ref_df %>% purrr::pluck(var) %>% sjlabelled::get_labels()
      x = x %>% 
        haven::labelled(labels = labs)
    }
    
  }
  
  return(x)
}