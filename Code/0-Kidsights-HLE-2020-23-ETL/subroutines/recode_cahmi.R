
recode_cahmi<-function(path, cahmi_2016_2020 = NULL, ...){

  library(haven)
  library(tidyverse)
  library(sjlabelled)
  library(spatstat)
  library(Hmisc)
  
  if(is.null(cahmi_2016_2020)){cahmi_2016_2020 = do.call(what = "combine_cahmi_datasets", args = list(path, ...))}
  
  xwalk_nsch = get_cahmi_xwalk(cahmi_2016_2020)
  
  nsch_ne = cahmi_2016_2020  %>% 
    mutate(study = "NSCH" %>% haven::labelled(label = "Study (i.e., data source)")) %>% 
    mutate(year = year %>% zap_attributes() %>% as.numeric() %>% haven::labelled(label = "Survey Year")) %>% 
    mutate(fipsst = fipsst %>% zap_attributes() %>% as.numeric() %>% haven::labelled(label = "State (FIPS code)")) %>% 
    mutate(uid = paste(study,year,fipsst,hhid,sep="-") %>% haven::labelled(label = "Unique identifier")) %>% 
    mutate(wgt = fwc/2 %>% haven::labelled(label = "Person weight")) %>% 
    mutate(hhid = hhid %>% as.character() %>% haven::labelled(label = "Household ID")) %>% 
    mutate(famid = NA %>% as.character() %>% haven::labelled(label = "Family identifier") ) %>% 
    mutate(puma = NA %>% as.character() %>% haven::labelled(label = "Public Use Micro Area")) %>% 
    mutate(rural = rural %>% as.numeric()) %>% 
    mutate(pctmetro = NA %>% as.numeric() %>% haven::labelled(label = "Percentage of PUMA Population in Metropolitan Area")) %>% 
    mutate(fwc = fwc %>% 
             as.numeric() %>% haven::labelled(label = "Child weight in NSCH population (i.e., weighted quintile)")) %>%  
    mutate(perwt = NA %>% as.numeric() %>% haven::labelled(label = "ACS Person Weights"))
    
  nsch_ne = nsch_ne %>%   
    mutate(famcount = famcount) %>% 
    mutate(fpl_i1 = fpl_i1) %>% 
    mutate(foodstamp_rc = foodstamp %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from=xwalk_nsch$foodstamp_rc$xwalk$foodstamp,to= xwalk_nsch$foodstamp_rc$xwalk$foodstamp_rc)%>% 
             haven::labelled(label = xwalk_nsch$foodstamp_rc$var_lab, 
                             labels = xwalk_nsch$foodstamp_rc$val_labs
                             )
            ) %>% 
    mutate(medicare_rc = NA %>% as.numeric() %>%  
             zap_attributes() %>% 
             plyr::mapvalues(from=xwalk_nsch$medicare_rc$xwalk$medicare,to= xwalk_nsch$medicare_rc$xwalk$medicare_rc)%>% 
             haven::labelled(label = xwalk_nsch$medicare_rc$var_lab, 
                             labels = xwalk_nsch$medicare_rc$val_labs
             )
    ) %>% 
    mutate(govhealthc_rc = govhealthc %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from=xwalk_nsch$govhealthc_rc$xwalk$govhealthc,to= xwalk_nsch$govhealthc_rc$xwalk$govhealthc_rc)%>% 
             haven::labelled(label = xwalk_nsch$govhealthc_rc$var_lab, 
                             labels = xwalk_nsch$govhealthc_rc$val_labs
             )
    ) %>% 
    mutate(wic_rc = wic %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from=xwalk_nsch$wic_rc$xwalk$wic,to= xwalk_nsch$wic_rc$xwalk$wic_rc)%>% 
             haven::labelled(label = xwalk_nsch$wic_rc$var_lab, 
                             labels = xwalk_nsch$wic_rc$val_labs
             )
    ) %>% 
    mutate(cashass_rc = cashass %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from=xwalk_nsch$cashass_rc$xwalk$cashass,to= xwalk_nsch$cashass_rc$xwalk$cashass_rc)%>% 
             haven::labelled(label = xwalk_nsch$cashass_rc$var_lab, 
                             labels = xwalk_nsch$cashass_rc$val_labs
             )
    ) %>% 
    mutate(mealfree_rc = mealfree %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from=xwalk_nsch$mealfree_rc$xwalk$mealfree,to= xwalk_nsch$mealfree_rc$xwalk$mealfree_rc)%>% 
             haven::labelled(label = xwalk_nsch$mealfree_rc$var_lab, 
                             labels = xwalk_nsch$mealfree_rc$val_labs
             )
    ) %>% 
    mutate(a1_age = a1_age, a2_age = a2_age) %>% 
    mutate(a1_female = a1_sex %>%
             zap_attributes() %>%
             plyr::mapvalues(from=xwalk_nsch$afemale$xwalk$asex,to= xwalk_nsch$afemale$xwalk$afemale) %>% 
             haven::labelled(labels = xwalk_nsch$afemale$val_labs, 
                             label = paste0("Adult 1 - ",xwalk_nsch$afemale$var_lab))
             ) %>% 
    mutate(a2_female = a2_sex %>%
             zap_attributes() %>%
             plyr::mapvalues(from=xwalk_nsch$afemale$xwalk$asex,to= xwalk_nsch$afemale$xwalk$afemale) %>% 
             haven::labelled(labels = xwalk_nsch$afemale$val_labs, 
                             label = paste0("Adult 2 - ",xwalk_nsch$afemale$var_lab))
    ) %>% 
    mutate(a1_edu4 = a1_grade %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$edu4$xwalk$grade, to = xwalk_nsch$edu4$xwalk$edu4) %>% 
             haven::labelled(label = paste0("Adult 1 - ", xwalk_nsch$edu4$var_lab), 
                             labels = xwalk_nsch$edu4$val_labs)
           ) %>% 
    mutate(
      a2_edu4 = a2_grade %>% 
        zap_attributes() %>% 
        plyr::mapvalues(from = xwalk_nsch$edu4$xwalk$grade, to = xwalk_nsch$edu4$xwalk$edu4) %>% 
        haven::labelled(label = paste0("Adult 2- ", xwalk_nsch$edu4$var_lab), 
                        labels = xwalk_nsch$edu4$val_labs)
    ) %>% 
    mutate(a1_married = a1_marital %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$married$xwalk$marital, 
                             to = xwalk_nsch$married$xwalk$married) %>% 
             haven::labelled(label = paste0("Adult 1 -", xwalk_nsch$married$var_lab), 
                             labels = xwalk_nsch$married$val_labs))%>% 
    mutate(a2_married = a2_marital %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$married$xwalk$marital, 
                             to = xwalk_nsch$married$xwalk$married) %>% 
             haven::labelled(label = paste0("Adult 2 -", xwalk_nsch$married$var_lab), 
                             labels = xwalk_nsch$married$val_labs)) %>% 
    mutate(a1_parent = a1_relation %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$parent$xwalk$relation, 
                             to = xwalk_nsch$parent$xwalk$parent) %>% 
             haven::labelled(label = paste0("Adult 1 - ", xwalk_nsch$parent$var_lab), 
                             labels = xwalk_nsch$parent$val_labels)) %>% 
    mutate(a2_parent = a2_relation %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$parent$xwalk$relation, 
                             to = xwalk_nsch$parent$xwalk$parent) %>% 
             haven::labelled(label = paste0("Adult 2 - ", xwalk_nsch$parent$var_lab), 
                             labels = xwalk_nsch$parent$val_labels))
  
  nsch_ne = nsch_ne %>% 
    mutate(female = sex %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$female$xwalk$sex, 
                             to = xwalk_nsch$female$xwalk$female) %>% 
             haven::labelled(label = xwalk_nsch$female$var_labs, 
                             labels = xwalk_nsch$female$val_labs)
    ) %>% 
    mutate(acedivorce_rc = acedivorce %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$acedivorce_rc$xwalk$acedivorce, 
                             to = xwalk_nsch$acedivorce_rc$xwalk$acedivorce_rc) %>% 
             haven::labelled(label = xwalk_nsch$acedivorce_rc$var_labs, 
                             labels = xwalk_nsch$acedivorce_rc$val_labs)
    ) %>% 
    mutate(acedeath_rc = acedeath %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$acedeath_rc$xwalk$acedeath, 
                             to = xwalk_nsch$acedeath_rc$xwalk$acedeath_rc) %>% 
             haven::labelled(label = xwalk_nsch$acedeath_rc$var_labs, 
                             labels = xwalk_nsch$acedeath_rc$val_labs)
    ) %>% 
    mutate(acejail_rc = acejail %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$acejail_rc$xwalk$acejail, 
                             to = xwalk_nsch$acejail_rc$xwalk$acejail_rc) %>% 
             haven::labelled(label = xwalk_nsch$acejail_rc$var_labs, 
                             labels = xwalk_nsch$acejail_rc$val_labs)
    ) %>% 
    mutate(acedv_rc = acedv %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$acedv_rc$xwalk$acedv, 
                             to = xwalk_nsch$acedv_rc$xwalk$acedv_rc) %>% 
             haven::labelled(label = xwalk_nsch$acedv_rc$var_labs, 
                             labels = xwalk_nsch$acedv_rc$val_labs)
    ) %>% 
    mutate(aceviolence_rc = aceviolence %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$aceviolence_rc$xwalk$aceviolence, 
                             to = xwalk_nsch$aceviolence_rc$xwalk$aceviolence_rc) %>% 
             haven::labelled(label = xwalk_nsch$aceviolence_rc$var_labs, 
                             labels = xwalk_nsch$aceviolence_rc$val_labs)
    ) %>% 
    mutate(acementalill_rc = acementalill %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$acementalill_rc$xwalk$acementalill, 
                             to = xwalk_nsch$acementalill_rc$xwalk$acementalill_rc) %>% 
             haven::labelled(label = xwalk_nsch$acementalill_rc$var_labs, 
                             labels = xwalk_nsch$acementalill_rc$val_labs)
    ) %>% 
    mutate(aceaddict_rc = aceaddict %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$aceaddict_rc$xwalk$aceaddict, 
                             to = xwalk_nsch$aceaddict_rc$xwalk$aceaddict_rc) %>% 
             haven::labelled(label = xwalk_nsch$aceaddict_rc$var_labs, 
                             labels = xwalk_nsch$aceaddict_rc$val_labs)
    ) %>% 
    mutate(aceracism_rc = aceracism %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$aceracism_rc$xwalk$aceracism, 
                             to = xwalk_nsch$aceracism_rc$xwalk$aceracism_rc) %>% 
             haven::labelled(label = xwalk_nsch$aceracism_rc$var_labs, 
                             labels = xwalk_nsch$aceracism_rc$val_labs)
    ) %>% 
    mutate(care10hrs_rc = (care10hrs==1) %>% 
             as.numeric() %>%  
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$care10hrs_rc$xwalk$care10hrs, 
                             to = xwalk_nsch$care10hrs_rc$xwalk$care10hrs_rc) %>% 
             haven::labelled(label = xwalk_nsch$care10hrs_rc$var_labs, 
                             labels = xwalk_nsch$care10hrs_rc$val_labs)
    ) %>% 
    mutate(race4_char = race4 %>% 
             zap_attributes() %>% 
             plyr::mapvalues(from = xwalk_nsch$race4_char$xwalk$race4, 
                             to = xwalk_nsch$race4_char$xwalk$race4_char) %>% 
             haven::labelled(label = xwalk_nsch$race4_char$var_labs, 
                             labels = xwalk_nsch$race4_char$val_labs)
    ) %>% 
    mutate(black_any = NA %>% as.numeric() %>% haven::labelled(label = "Any Black/African American identified", labels = c(No=0,Yes=1)), 
           hisp = (race4_char=="Hispanic") %>% as.numeric() %>% haven::labelled(label = "Hispanic", labels = c(No=0,Yes=1))) %>% 
    mutate(mtorace4_char = NA %>% as.character()) %>% 
    mutate(inc99 = NA %>% as.numeric() %>% haven::labelled(label = "Total household income in 1999 USD"), 
           cpi99 = get_cpi99(year) %>% as.numeric() %>% haven::labelled(label = "CPI-U adjustment factor to 1999 dollars"), 
           famcount = famcount)
  
  vars_df = readxl::read_excel(path=paste0(path,"/data-files/intermediate-files/gameplan-combined-acs-nsch.xlsx"), sheet = "vars")
  
  
  nsch_ne = nsch_ne %>% dplyr::select(all_of(vars_df$var_names))
  
  return(nsch_ne)

}


