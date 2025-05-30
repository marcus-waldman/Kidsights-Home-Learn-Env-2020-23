describe_stat_in_demo<-function(acs,cahmi,survey, print_tables = T, do_within = F, format = NULL){
  
  library(tidyverse)
  
  
  df = list(
    acs,
    cahmi,
    survey %>% 
      dplyr::mutate(fpl_i1 = 100*(inc99/get_cpi99(2022))/26500.00) %>% 
      dplyr::mutate(fpl_i1 = ifelse(fpl_i1<50, 50, ifelse(fpl_i1>400,400, fpl_i1)))
  ) %>% 
  dplyr::bind_rows()
  
  
  df = df %>% 
    dplyr::mutate(
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
      urban = 1-rural, 
      log_inc99 = log10(inc99+1),
      care10hrs_rc = as.integer(care10hrs_rc==1)
    )
  
  
  # set urban and rural to NA for acs
  df$rural[startsWith(df$study,"ACS")] = NA
  df$urban[startsWith(df$study,"ACS")] = NA
  
  # set the wgt variable to 1 for unmc
  df$wgt[startsWith(df$study,"UNMC")]=1
  
  race_vars = c("white","black","hisp")
  income_vars = c("below_fpl185","above_fpl185")
  edu_vars = c("no_bachelors","bachelors")
  loc_vars = c("rural","urban")
  other_vars = c("log_inc99","fpl_i1","a1_age","a1_menthealth","a1_married","chhealth","care10hrs_rc","no_hs","hs","some_college")
  all_vars = c(race_vars, income_vars, edu_vars, loc_vars)
  studies = c("ACS","NSCH","UNMC")
  
  
  df2 = df %>% dplyr::select(study,wgt,all_of(all_vars))
  df2$study = as.factor(df$study)
  df2$study = relevel(df2$study, ref = "UNMC22")

#### Look overall ####
  df_v =     df[,c("wgt","study",all_vars,other_vars)]
  studies = c("ACS","NSCH","UNMC")
  
  tab_overall = get_demo_tabout( df[,c("wgt","study",all_vars,other_vars)], caption = "Means: 2022 UNMC demographics vs. 5-yr ACS/NSCH", format = format)
  if(print_tables){
    print(tab_overall)
  }

  
####### Look within demographics #######
out_list = NULL
if (do_within){
    out_list<-lapply(1:length(all_vars), function(v){
        var_v  = all_vars[v] 
        complement_v = c(all_vars[-v], other_vars)
        
        
        df_v = df[which(df[,var_v]==1),c("wgt","study",complement_v)]
        
        tabout =get_demo_tabout( df_v, complement_v = complement_v,
                                 caption = paste0(var_v,": Means: 2022 UNMC demographics vs. 5-yr ACS/NSCH"),
                                 format = format)
        
        
        if(print_tables){
          print(tabout)
        }
        return(tabout)
    
  })
 names(out_list) = all_vars
}  
  return(list(overall = tab_overall, stratified = out_list))  
  
}
 
 
 

