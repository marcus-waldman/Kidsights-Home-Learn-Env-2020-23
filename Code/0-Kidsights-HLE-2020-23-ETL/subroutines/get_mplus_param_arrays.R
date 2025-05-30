get_mplus_param_arrays<-function(mplus_file){
  
  require(tidyverse)
  require(MplusAutomation)
  
  obj_Mplus = MplusAutomation::readModels(filefilter = mplus_file)
  
  BETA = obj_Mplus$parameters$unstandardized %>% dplyr::filter(startsWith(paramHeader,"F.ON"))
  
  GAMMA = obj_Mplus$parameters$unstandardized %>% dplyr::filter(startsWith(param,"GAMMA"))
  
  ALPHA = obj_Mplus$parameters$unstandardized %>% dplyr::filter(startsWith(paramHeader,"F.BY"))
  
  TAU1 = obj_Mplus$parameters$unstandardized %>%
    dplyr::filter(startsWith(paramHeader,"Thresholds")) %>%
    dplyr::filter(endsWith(param,"$1")) %>%
    dplyr::mutate(lex_kidsight = str_remove_all(param,"\\$1"),
                  tau1 = est) %>%
    dplyr::select(lex_kidsight, tau1)
  
  
  TAU2 = obj_Mplus$parameters$unstandardized %>%
    dplyr::filter(startsWith(paramHeader,"Thresholds")) %>%
    dplyr::filter(endsWith(param,"$2")) %>%
    dplyr::mutate(lex_kidsight = str_remove_all(param,"\\$2"),
                  tau2 = est) %>%
    dplyr::select(lex_kidsight, tau2)
  
  
  TAU3 = obj_Mplus$parameters$unstandardized %>%
    dplyr::filter(startsWith(paramHeader,"Thresholds")) %>%
    dplyr::filter(endsWith(param,"$3")) %>%
    dplyr::mutate(lex_kidsight = str_remove_all(param,"\\$3"),
                  tau3 = est) %>%
    dplyr::select(lex_kidsight, tau3)
  
  TAU4 = obj_Mplus$parameters$unstandardized %>%
    dplyr::filter(startsWith(paramHeader,"Thresholds")) %>%
    dplyr::filter(endsWith(param,"$4")) %>%
    dplyr::mutate(lex_kidsight = str_remove_all(param,"\\$4"),
                  tau4 = est) %>%
    dplyr::select(lex_kidsight, tau4)
  
  TAU5 = obj_Mplus$parameters$unstandardized %>%
    dplyr::filter(startsWith(paramHeader,"Thresholds")) %>%
    dplyr::filter(endsWith(param,"$5")) %>%
    dplyr::mutate(lex_kidsight = str_remove_all(param,"\\$5"),
                  tau5 = est) %>%
    dplyr::select(lex_kidsight, tau5)
  
  
  TAU = TAU1  %>%
    dplyr::left_join(TAU2, by = "lex_kidsight") %>%
    dplyr::left_join(TAU3, by = "lex_kidsight") %>%
    dplyr::left_join(TAU4, by = "lex_kidsight") %>%
    dplyr::left_join(TAU5, by = "lex_kidsight")
  
  
  
  K = apply(TAU %>% dplyr::select(tau1:tau5), 1, function(x){sum(!is.na(x))+1})
  
  TAU = TAU %>% dplyr::mutate(K = K)
  
  return(list(BETA = BETA, GAMMA = GAMMA, ALPHA = ALPHA, TAU = TAU, obj = obj_Mplus))

}
