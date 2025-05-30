# rm(list = ls())
# 
# library(tidyverse)
# library(ggmice)
# library(pbapply)
# library(readxl)
# library(parallel)
# 
# path = "C:/Users/marcu/Dropbox/UNMC/phase-2"
# 
# subroutines = list.files(paste0(path,"/subroutines"), pattern = ".R", full.names = T)
# for(x in 1:length(subroutines)){source(file = subroutines[x])}
# 
# cahmi_2016_2020 = readr::read_rds(file = paste0(path, "/data-files/intermediate-files/cahmi_2016_2020.RDS")) %>% 
#   dplyr::filter(fipsst == 31) %>%  
#   recode_cahmi(path = path, cahmi_2016_2020 = .)
# 
# acs_2016_2020 = readr::read_rds(paste0(path,"/data-files/intermediate-files/acs5_2020.RDS")) %>%
#   dplyr::filter(STATEFIP == 31) %>%
#   recode_acs(path = path, ref_df = cahmi_2016_2020)
# 
# # adult_acs_2016_2020 = readr::read_rds(paste0(path,"/data-files/intermediate-files/acs5_2020.RDS")) %>%
# #   dplyr::filter(STATEFIP == 31) %>%
# #   recode_acs(path = path, ref_df = cahmi_2016_2020, inclusive = T) %>% 
# #   dplyr::mutate(keep = (famid %in% acs_2016_2020$famid) ) %>% 
# #   dplyr::filter(keep) %>% 
# #   dplyr::select(uid,famid,PERNUM:POPLOC2,a1_edu4, a1_age,a1_female)
# # 
# # 
# # cl = parallel::makeCluster(8)
# # clusterExport(cl, c("acs_2016_2020", "adult_acs_2016_2020"))
# # 
# # caregivers_list = pblapply(1:nrow(acs_2016_2020), function(x){
# #   require(tidyverse)
# #   uid_x = acs_2016_2020$uid[x]
# #   fid_x = acs_2016_2020$famid[x]
# #   parnums_x = adult_acs_2016_2020 %>% dplyr::filter(uid == uid_x) %>% dplyr::select(MOMLOC:POPLOC2) %>% as.numeric()
# #   adult_acs_2016_2020 %>% dplyr::filter(famid == fid_x, !is.na(a1_age)) %>% 
# #     dplyr::mutate(a1_parent = as.integer(PERNUM %in% parnums_x), 
# #                   uid = uid_x) %>% 
# #     dplyr::select(PERNUM,uid,a1_edu4,a1_age,a1_female,a1_parent) %>% 
# #   return()
# # }, cl=cl)
# # 
# # parallel::stopCluster(cl)
# # 
# # 
# # saveRDS(caregivers_list, file = paste0(path,"/data-files/intermediate-files/caregivers_list.RDS"))
# # caregivers_list = readr::read_rds(file =paste0(path,"/data-files/intermediate-files/caregivers_list.RDS"))
# # caregivers_df = caregivers_list %>% 
# #   data.table::rbindlist() %>% 
# #   dplyr::mutate(a1type = ifelse(a1_parent==1 & a1_female ==1, 0, ifelse(a1_parent==1 & a1_female==0, 1, 2))) %>% 
# #   dplyr::arrange(uid, a1type)
# 
# 
# 
# 
# 
# M = 10
# iters = 20
# t1 = proc.time()
impute_nsch_cahmi<-function(acs_2016_2020, cahmi_2016_2020, 
                            M=10, iters = 20,  path = "C:/Users/marcu/Dropbox/UNMC/phase-2"){
  
  require(tidyverse)  
  require(mice)
  require(pbapply)
  require(parallel)
  require(readr)
  require(ranger)
  
  caregivers_list = readr::read_rds(file =paste0(path,"/data-files/intermediate-files/caregivers_list.RDS"))
  caregivers_df = caregivers_list %>% 
    data.table::rbindlist() %>% 
    dplyr::mutate(a1type = ifelse(a1_parent==1 & a1_female ==1, 0, ifelse(a1_parent==1 & a1_female==0, 1, 2))) %>% 
    dplyr::arrange(uid, a1type)
  
  fpl100_df = readxl::read_excel(paste0(path,"/data-files/source-files/federal_poverty_guidelines.xlsx"), sheet = "fpl100")
  
  tmp = cahmi_2016_2020 %>% 
      dplyr::select(uid,year, famcount,fpl_i1) %>% 
      dplyr::left_join(fpl100_df, by = c("year","famcount")) %>% 
      dplyr::mutate(hhinc = fpl100*(fpl_i1/100), 
                    cpi99 = get_cpi99(yr = year), 
                    inc99 = hhinc*cpi99) %>% 
      dplyr::select(uid,inc99)
    
    cahmi_2016_2020 = cahmi_2016_2020 %>% 
      dplyr::select(-inc99) %>% 
      dplyr::left_join(tmp, by = c("uid")) %>% 
      relocate(names(acs_2016_2020))
    
    
    
    
    
    # Impute ACS
    vars_impute_acs = c("wgt","year","pctmetro","inc99", "fpl_i1","famcount", "foodstamp_rc", "medicare_rc", "govhealthc_rc", "cashass_rc", "race4_char", "sc_age_years", "female")
    tmp_acs <-acs_2016_2020 %>% 
      dplyr::select(all_of(vars_impute_acs)) %>% 
      zap_attributes() 
    mice_acs = mice::parlmice(data = tmp_acs, cluster.seed = 42, n.core = M, n.imp.core = 1, method = "rf", maxit = iters)
    
    acs_implist = pblapply(1:M, 
                      function(m){complete(mice_acs,m) %>% 
                                  dplyr::mutate(race4_char = as.character(race4_char), 
                                                uid = acs_2016_2020$uid) %>%
                                  dplyr::left_join(acs_2016_2020 %>% dplyr::select(-all_of(vars_impute_acs)),
                                                  by = "uid") %>% 
                                  dplyr::select(all_of(names(acs_2016_2020))) 
                                  })
    
    
    # Impute NSCH
    vars_impute_nsch = c("year", 
                         "wgt", 
                         "rural", 
                         "inc99", 
                         "famcount", 
                         "fpl_i1", 
                         "foodstamp_rc", 
                         "govhealthc_rc", 
                         "wic_rc", 
                         "cashass_rc", 
                         "mealfree_rc", 
                         "a1_age", 
                         "a1_female", 
                         "a1_edu4", 
                         "a1_married", 
                         "a1_menthealth", 
                         "a1_physhealth", 
                         "a1_parent", 
                         "a2_age", 
                         "a2_female", 
                         "a2_edu4", 
                         "a2_married", 
                         "a2_menthealth", 
                         "a2_physhealth", 
                         "a2_parent", 
                         "sc_age_years", 
                         "female", 
                         "race4_char",
                         "acedivorce_rc", 
                         "acedeath_rc", 
                         "acejail_rc", 
                         "acedv_rc", 
                         "aceviolence_rc", 
                         "acementalill_rc", 
                         "aceaddict_rc", 
                         "aceracism_rc", 
                         "chhealth", 
                         "care10hrs_rc")
    
    tmp_nsch = cahmi_2016_2020 %>% 
      dplyr::mutate(race4_char = as.factor(race4_char)) %>% 
      dplyr::select(all_of(vars_impute_nsch)) %>% 
      zap_attributes()
    mice_nsch <- mice::parlmice(data = tmp_nsch, cluster.seed = 42, n.core = M, n.imp.core = 1, method = "rf", maxit = iters)
    
    nsch_implist =  pblapply(1:M, 
                             function(m){complete(mice_nsch,m) %>% 
                                 dplyr::mutate(race4_char = as.character(race4_char), 
                                               uid = cahmi_2016_2020$uid) %>%
                                 dplyr::left_join(cahmi_2016_2020 %>% dplyr::select(-all_of(vars_impute_nsch)),
                                                  by = "uid") %>% 
                                 dplyr::select(all_of(names(acs_2016_2020))) 
                             })
    
    
    
    # Combined imputation
    vars_impute = c("study","rural","pctmetro","inc99","famcount","fpl_i1",
                    "foodstamp_rc","medicare_rc","govhealthc_rc","wic_rc","cashass_rc", "mealfree_rc", 
                    "sc_age_years","female","race4_char","care10hrs_rc", 
                    "acedivorce_rc", "acedeath_rc", "acejail_rc", "acedv_rc", "aceviolence_rc", "acementalill_rc", "aceaddict_rc", "aceracism_rc", 
                    "a1_age","a1_female","a1_edu4", "a1_menthealth","a1_physhealth","a1_parent") 
    cl = parallel::makeCluster(M)
    clusterExport(cl, c("acs_implist", "nsch_implist", "caregivers_df", "vars_impute", "iters", "acs_2016_2020"))
    
    acs_nsch_implist<-pblapply(1:M, function(m){
      
      require(tidyverse)
      require(mice)
      require(ranger)
      
      combined_m0 = acs_implist[[m]] %>% 
        dplyr::bind_rows(nsch_implist[[m]]) %>% 
        dplyr::select(uid, all_of(vars_impute)) %>% 
        dplyr::mutate(across(where(is.character), as.factor))
      
      df_init = NULL
      
      for(i in 1:iters){
        combined_m1 = combined_m0 %>% 
          dplyr::select(all_of(vars_impute)) %>% 
          mice::mice(method = "rf", maxit = 1, m = 1, remove.collinear = F, data.init = df_init, visitSequence = "roman")  %>% 
          complete(1) %>%  
          dplyr::mutate(uid = combined_m0$uid) %>% 
          relocate(names(combined_m0))
        
        combined_m1$a1_age[combined_m1$study == "ACS5-2020"]  = NA
        combined_m1$a1_female[combined_m1$study == "ACS5-2020"]  = NA
        combined_m1$a1_edu4[combined_m1$study == "ACS5-2020"]  = NA
        combined_m1$a1_menthealth[combined_m1$study == "ACS5-2020"]  = NA
        combined_m1$a1_physhealth[combined_m1$study == "ACS5-2020"]  = NA
        combined_m1$a1_parent[combined_m1$study == "ACS5-2020"]  = NA  
        
        # Generate an initial imputation
        combined_m1 = combined_m1 %>% 
          mutate(a1type = ifelse(a1_parent==1 & a1_female ==1, 0, ifelse(a1_parent==1 & a1_female==0, 1, 2)))
        
        obj_m1 = ranger::ranger(a1type~.,
                                probability = T, 
                                data = combined_m1 %>% 
                                  dplyr::select(-starts_with("a1_")) %>% 
                                  dplyr::filter(!is.na(a1type)) %>% 
                                  sample_frac(1, replace = T)
        )
        
        
        
        foo = predict(obj_m1, data = combined_m1 %>% dplyr::filter(is.na(a1type))) %>% 
          purrr::pluck("predictions") %>% 
          Hmisc::rMultinom(1) %>% 
          as.numeric()
        
        bar = combined_m1 %>% dplyr::filter(is.na(a1type)) %>%  dplyr::select(uid) %>% 
          dplyr::mutate(a1type_hat = foo-1)
        
        foo = caregivers_df %>% 
          dplyr::left_join(bar %>% dplyr::mutate(uid = as.character(uid)), by = "uid") %>% 
          dplyr::mutate(a1star = abs(a1type-a1type_hat)) %>% 
          dplyr::select(a1star, a1type, uid:a1_parent, a1star) %>% 
          dplyr::arrange(uid,a1star) %>% 
          dplyr::group_by(uid) %>% 
          dplyr::summarize(a1_edu4 = a1_edu4[1], a1_age = a1_age[1], a1_female = a1_female[1], a1_parent = a1_parent[1])
        
        bar = combined_m1 %>% dplyr::filter(is.na(a1type)) %>%  dplyr::select(uid) %>% 
          dplyr::mutate(uid = as.character(uid)) %>% 
          dplyr::left_join(foo, by = "uid")
        
        combined_m2 = combined_m1
        Nm2 = nrow(bar)
        combined_m2$a1_edu4[1:Nm2] = bar$a1_edu4
        combined_m2$a1_parent[1:Nm2] = bar$a1_parent
        combined_m2$a1_female[1:Nm2] = bar$a1_female
        combined_m2$a1_age[1:Nm2] = bar$a1_age
        
        df_init = combined_m2 %>% 
          dplyr::select(-a1type,-uid) %>% 
          mice::mice(method = "rf", m = 1, maxit = 1) %>% 
          mice::complete(1) %>% 
          dplyr::mutate(uid = combined_m2$uid) %>% 
          dplyr::select(names(combined_m0))
      }
      
      combined_m = df_init %>% 
        dplyr::mutate(race4_char = as.character(race4_char), 
                      uid = as.character(uid)) %>%
        dplyr::left_join(acs_implist[[1]] %>% 
                           dplyr::bind_rows(nsch_implist[[1]]) %>% 
                           dplyr::select(-all_of(vars_impute)),
                         by = "uid") %>% 
        dplyr::select(all_of(names(acs_2016_2020)))
      
      return(combined_m)
      
    }, cl = cl)
    
    parallel::stopCluster(cl)
    
    
    readr::write_rds(acs_nsch_implist, file = paste0(path,"/data-files/intermediate-files/acs_nsch_implist.RDS"))
    # 
    # t2 = proc.time()-t1
    
    return(acs_nsch_implist)
}