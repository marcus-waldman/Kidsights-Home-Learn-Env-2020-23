get_demo_tabout<-function(df_v, complement_v, caption = NULL, studies = c("ACS","NSCH","UNMC"), format = "simple"){
  
  library(tidyverse)
  
  
  ps_v = lapply(1:3, function(s){
    hi = df_v %>% 
      dplyr::filter(startsWith(study,studies[s]))
    if(nrow(hi)>0){
      wgts_v = hi$wgt
      bye = apply(hi %>% dplyr::select(-"study",-"wgt"), 2, "weighted.mean", na.rm = T, w=wgts_v) %>% 
        data.frame()
      names(bye) = studies[s]
    } else {
      bye = data.frame(hi = rep(as.numeric(NA), length(complement_v)))
      names(bye) = studies[s]
    }
    return(bye)
  }) %>% dplyr::bind_cols()
  
  ns_v = lapply(1:3, function(s){
    hi = df_v %>% 
      dplyr::filter(startsWith(study,studies[s]))
    if(nrow(hi)>0){
      wgts_v = hi$wgt
      bye = apply(hi %>% dplyr::select(-"study",-"wgt"), 2, function(x){sum(!is.na(x))}) %>% 
        data.frame()
      names(bye) = studies[s]
    } else {
      bye = data.frame(hi = rep(as.numeric(NA), length(complement_v)))
      names(bye) = studies[s]
    }
    return(bye)
  }) %>% dplyr::bind_cols()
  
  ses_v = lapply(1:3, function(s){
    hi = df_v %>% 
      dplyr::filter(startsWith(study,studies[s]))
    if(nrow(hi)>0){
      wgts_v = hi$wgt
      bye = apply(hi %>% dplyr::select(-"study",-"wgt"), 2, function(x){diagis::weighted_se(x,w=wgts_v, na.rm = T)}) %>% 
        data.frame()
      names(bye) = studies[s]
    } else {
      bye = data.frame(hi = rep(as.numeric(NA), length(complement_v)))
      names(bye) = studies[s]
    }
    return(bye)
  }) %>% dplyr::bind_cols()
  
  ci_ub_v = ps_v + 1.96*ses_v
  ci_lb_v = ps_v - 1.96*ses_v
  
  
  names(ses_v) = paste0("SE_",names(ses_v))
  names(ns_v) = paste0("N_", names(ns_v))
  tmp_v = cbind(ns_v,ps_v, ses_v)%>% 
    dplyr::mutate(d_ACS_NSCH = ACS-NSCH, 
                  d_UNMC_ACS = UNMC-ACS, 
                  d_UNMC_NSCH = UNMC - NSCH, 
                  se_ACS_NSCH = sqrt(SE_ACS^2 + SE_NSCH^2), 
                  se_UNMC_ACS = sqrt(SE_ACS^2 + SE_UNMC^2), 
                  se_UNMC_NSCH = sqrt(SE_NSCH^2 + SE_UNMC^2),
                  z_ACS_NSCH = d_ACS_NSCH/se_ACS_NSCH, 
                  z_UNMC_ACS = d_UNMC_ACS/se_UNMC_ACS, 
                  z_UNMC_NSCH =d_UNMC_NSCH/se_UNMC_NSCH,
                  alpha_ACS_NSCH = sum(!is.na(z_ACS_NSCH)), 
                  alpha_UNMC_ACS = sum(!is.na(z_UNMC_ACS)), 
                  alpha_UNMC_NSCH = sum(!is.na(z_UNMC_NSCH)),
                  pval_ACS_NSCH = 2*alpha_ACS_NSCH*pnorm(-abs(z_ACS_NSCH)), 
                  pval_UNMC_ACS = 2*alpha_UNMC_ACS*pnorm(-abs(z_UNMC_ACS)), 
                  pval_UNMC_NSCH =2*alpha_UNMC_NSCH*pnorm(-abs(z_UNMC_NSCH))) %>% 
    dplyr::mutate(across(starts_with("pval"), function(x){2*ifelse(x>.5,1-x,x)})) %>% 
    dplyr::mutate(ACS_NSCH = paste0(as.character(num2txt(d_ACS_NSCH)),get_stars(pval_ACS_NSCH)), 
                  UNMC_ACS = paste0(as.character(num2txt(d_UNMC_ACS)),get_stars(pval_UNMC_ACS)), 
                  UNMC_NSCH = paste0(as.character(num2txt(d_UNMC_NSCH)),get_stars(pval_UNMC_NSCH))) %>% 
    dplyr::select(UNMC_ACS, UNMC_NSCH, N_NSCH,N_ACS,N_UNMC,NSCH,ACS,UNMC,ACS_NSCH) %>% 
    dplyr::mutate(across(where(is.numeric), function(x){num2txt(x)}))
  
  
  tabout = knitr::kable(tmp_v, label = caption, format = format)
  
  return(tabout)
}

