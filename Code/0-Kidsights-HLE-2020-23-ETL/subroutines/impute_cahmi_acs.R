impute_cahmi_acs<-function(path, M = 16, maxit=10, cores=8, save_it = F){
  
  library(parallel)
  library(pbapply)
  
  cahmi = readr::read_rds(file = paste0(path, "/data-files/intermediate-files/cahmi_2016_2020.RDS")) %>%
    dplyr::filter(fipsst == 31) %>%
    recode_cahmi(path = path, cahmi_2016_2020 = .)
  
  inclusive_acs = readr::read_rds(paste0(path,"/data-files/intermediate-files/acs5_2020.RDS")) %>%
    dplyr::filter(STATEFIP == 31) %>%
    recode_acs(path = path, ref_df = cahmi, inclusive = T)
  
  parents_list_acs = get_parents_acs_list(acs = inclusive_acs)
  
  acs = inclusive_acs %>% 
    dplyr::filter(sc_age_years<18) %>% 
    dplyr::select(all_of(names(cahmi)))
  
  dict = get_demographic_dict(cahmi, acs)
  
  cl = parallel::makeCluster(cores)
  parallel::clusterExport(cl, varlist = c("cahmi","acs","dict","maxit","parents_list_acs"), envir=environment())
  imp_list <- pblapply(1:M, function(x){
    
    library(tidyverse)
    library(mice)
    
    # Impute CAHMI
    dict.cahmi = dict %>% dplyr::filter(in_cahmi, var!="study", var!="wgt", impute_block!="")
    tmp = cahmi %>% 
      dplyr::select(all_of(dict.cahmi$var)) %>% 
      mutate(across(where(is.character),as.factor))
    obj_mice_cahmi = mice(data = tmp, m = 1, maxit = maxit, method = "rf")
    
    
    # Impute ACS
    dict.acs = dict %>% dplyr::filter(in_acs, var!="study", var!="wgt", impute_block!="")
    tmp = acs %>% 
      dplyr::select(all_of(dict.acs$var)) %>% 
      mutate(across(where(is.character),as.factor))
    obj_mice_acs = mice(data = tmp, m = 1, maxit = maxit, method = "rf")
    
    
    cahmi_x = complete(obj_mice_cahmi,1) %>% 
      dplyr::mutate(uid = cahmi$uid) 
    vars_x = c("uid","study",setdiff(names(cahmi), names(cahmi_x)))
    cahmi_x = cahmi_x %>% 
      dplyr::left_join(cahmi %>% dplyr::select(all_of(vars_x)), by = "uid") %>% 
      dplyr::select(all_of(names(cahmi))) %>% 
      dplyr::mutate(across(where(is.factor), as.character))
    
    
    acs_x = complete(obj_mice_acs,1) %>% 
      dplyr::mutate(uid = acs$uid)
    vars_x = c("uid","study",setdiff(names(acs), names(acs_x)))
    acs_x = acs_x %>% 
      dplyr::left_join(acs %>% dplyr::select(all_of(vars_x)), by = "uid") %>% 
      dplyr::select(all_of(names(acs))) %>% 
      dplyr::mutate(across(where(is.factor), as.character))
    
    dict.combined = dict %>% 
      dplyr::filter(impute_block!="", var!="wgt", var!="fwc", var!="perwt") 
    
    combined = bind_rows(cahmi_x, acs_x)
    uids_df = combined %>% dplyr::select(uid)
    tmp = combined %>% 
      dplyr::select(all_of(dict.combined$var)) %>% 
      mutate(across(where(is.character),as.factor))
    
    init0 = tmp %>% missRanger::missRanger(pmm.k = 5)
    
    for(i in 1:maxit){
      
      obj_mice_combined = mice(tmp, m = 1, maxit = 1, method = "rf", init = init0)
      combined_iter = complete(mice(complete(obj_mice_combined,1), m = 1, maxit = 1, method = "rf", init = init0$rural),1) %>% 
        dplyr::mutate(uid = combined$uid)
      
      a1_moms_acs = combined_iter %>% 
        dplyr::filter(a1_parent==1, a1_female==1, uid %in% parents_list_acs$mom$uid) %>% 
        dplyr::select(all_of(names(parents_list_acs$mom)))
      
      a1_dads_acs = combined_iter %>% 
        dplyr::filter(a1_parent==1, a1_female==0, uid %in% parents_list_acs$dad$uid) %>% 
        dplyr::select(all_of(names(parents_list_acs$dad)))
      
      
      a1_acs = bind_rows(a1_moms_acs, a1_dads_acs) %>% 
        left_join(combined_iter %>% dplyr::select(-a1_age, -a1_female, -a1_edu4, -a1_married, -a1_parent), by = "uid") %>% 
        dplyr::select(all_of(names(combined_iter)))
      
      #nrow(combined_iter)
      combined_iter = combined_iter %>% 
        dplyr::filter(!(uid %in% a1_acs$uid)) %>% 
        bind_rows(a1_acs)
      #nrow(combined_iter)
      
      init0  = uids_df %>%
        dplyr::left_join(combined_iter, by = "uid") %>% 
        dplyr::select(all_of(names(tmp)))
      
    }
    vars_m = c("uid",setdiff(names(combined),names(init0)))
    combined_x = combined %>% 
      dplyr::select(all_of(vars_m)) %>% 
      left_join(init0 %>% dplyr::mutate(uid = uids_df$uid), by = "uid") %>% 
      dplyr::select(all_of(names(acs))) %>% 
      dplyr::mutate(.imp = x) %>% 
      dplyr::relocate(".imp") %>% 
      dplyr::select(-starts_with("a2_"))
    
    return(combined_x)
    
  },  cl=cl)
  parallel::stopCluster(cl)
  
  if(save_it){
    library(readr)
    readr::write_rds(imp_list, 
                     file = paste0(path,"/data-files/intermediate-files/imp_list_cahmi_acs.rds")
    )
  }
  
  return(imp_list)
  
  
}
