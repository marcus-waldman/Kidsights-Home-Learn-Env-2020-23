nom_fscores<-function(df, idvar = "ResponseId"){
  
  library(tidyverse)
  library(mirt)
  
  print("Scoring NOM...")
  
  # Early Learning
  learn_items = c("NOM002","NOM003","NOM005","NOM006", 
                  "NOM012","NOM014","NOM015","NOM017", 
                  "NOM018","NOM026")
  learn_df = df %>% 
    dplyr::select(any_of(c(idvar,learn_items))) %>% 
    clean_item_responses(item_names = learn_items)
  
  learn_df = learn_df %>% 
    dplyr::mutate(nomlearn = mirt::mirt(learn_df %>% dplyr::select(any_of(learn_items))) %>% 
                    mirt::fscores(method = "MAP") %>% 
                    as.vector())
  
  # Physical Development
  phys_items = c("NOM024","NOM027","NOM033","NOM034", 
                  "NOM042","NOM043","NOM044","NOM045", 
                  "NOM046")
  phys_df = df %>% 
    dplyr::select(any_of(c(idvar,phys_items))) %>% 
    clean_item_responses(item_names = phys_items)
  
  phys_df = phys_df %>% 
    dplyr::mutate(nomphys = mirt::mirt(phys_df %>% dplyr::select(any_of(phys_items))) %>% 
                    mirt::fscores(method = "MAP") %>% 
                    as.vector())
  
  
  # Socioemotional development
  sem_items = c("NOM047","NOM048","NOM049","NOM050", 
                "NOM052","NOM053")
  sem_df = df %>% 
    dplyr::select(any_of(c(idvar,sem_items))) %>% 
    clean_item_responses(item_names = sem_items)
  
  sem_df = sem_df %>% 
    dplyr::mutate(nomsem = mirt::mirt(sem_df %>% dplyr::select(any_of(sem_items))) %>% 
                    mirt::fscores(method = "MAP") %>% 
                    as.vector())
  
  
  # Self Regulation
  sreg_items = c("NOM054","NOM056","NOM057","NOM058", 
                 "NOM059","NOM060","NOM061","NOM062")
  sreg_df = df %>% 
    dplyr::select(any_of(c(idvar,sreg_items))) %>% 
    clean_item_responses(item_names = sreg_items)
  
  sreg_df = sreg_df %>% 
    dplyr::mutate(nomsreg = mirt::mirt(sreg_df %>% dplyr::select(any_of(sreg_items))) %>% 
                    mirt::fscores(method = "MAP") %>% 
                    as.vector())
  
  # Join in all scores
  out_df = df %>% dplyr::select(any_of(idvar)) %>% 
    dplyr::left_join(learn_df %>% dplyr::select(any_of(idvar), "nomlearn"), by = idvar) %>% 
    dplyr::left_join(phys_df %>% dplyr::select(any_of(idvar), "nomphys"), by = idvar) %>% 
    dplyr::left_join(sem_df %>% dplyr::select(any_of(idvar), "nomsem"), by = idvar) %>% 
    dplyr::left_join(sreg_df %>% dplyr::select(any_of(idvar), "nomsreg"), by = idvar) 
  
  
  return(out_df)
  
}
