recode_unmc2020<-function(survey_df,reference_df, cpi99=get_cpi99(2019)) {
  
    
    require(tidyverse)
    require(zipcodeR)
    require(stringr)
    
    
    edu4_xwalk = 
      list(
        xwalk = data.frame(
          orig_values = sjlabelled::get_labels(survey_df$CQR004),
          edu4 = c(0,0,1,2,2,2,3,3,3)
        ), 
        val_labs = c("Less than HS Diploma/GED"=0,"HS Diploma/GED"=1,"Some College, AA/AS, or Vocational/Trade/etc."=2,"BA/BS or Higher"=3), 
        var_lab = "Educational attainment (4 cat.; derived)"
      )
    
    
    recoded_df = survey_df %>% 
      dplyr::mutate(study = "UNMC20" %>% label_it("study", ref_df = reference_df),
                    year = 2022 %>% label_it("year", ref_df = reference_df),  
                    fipsst = 31 %>% label_it("fipsst", ref_df = reference_df), 
                    uid = as.character(mrwid) %>%  label_it("uid", ref_df = reference_df), 
                    wgt = NA %>% as.numeric() %>% label_it("wgt", ref_df = reference_df), 
                    hhid = as.character(NA) %>% label_it("hhid", ref_df = reference_df), 
                    famid = NA %>% as.character() %>% label_it("famid", ref_df = reference_df), 
                    zip = SQ001 %>% zipcodeR::normalize_zip() %>% zap_attributes(),
                    fwc = NA %>% as.numeric() %>% label_it("fwc", ref_df = reference_df), 
                    perwt = NA %>% as.numeric() %>% label_it("perwt", ref_df = reference_df), 
                    fpl_i1 = NA %>% as.numeric() %>% label_it("fpl_i1", ref_df = reference_df), 
                    foodstamp_rc = stringr::str_detect(CQR007,"SNAP") %>% 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("foodstamp_rc", ref_df = reference_df), 
                    medicare_rc = NA %>% 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("medicare_rc", ref_df = reference_df),
                    govhealthc_rc = stringr::str_detect(CQR007,"Medicaid") %>% 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("govhealthc_rc", ref_df = reference_df),
                    wic_rc = stringr::str_detect(CQR007,"WIC") %>% 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("wic_rc", ref_df = reference_df),
                    cashass_rc = stringr::str_detect(CQR007,"welfare") %>% 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("cashass_rc", ref_df = reference_df),
                    mealfree_rc = stringr::str_detect(CQR007,"breakfasts") %>% 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("mealfree_rc", ref_df = reference_df),
                    a1_age = CQR003 %>% 
                      as.integer() %>% 
                      ifelse(.>=75,75,.) %>% 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("a1_age", ref_df = reference_df), 
                    a1_female = (CQR002==2) %>% 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("a1_female", ref_df = reference_df), 
                    a1_edu4 = as.character(CQR004) %>% 
                      plyr::mapvalues(from = edu4_xwalk %>% purrr::pluck("xwalk") %>% purrr::pluck("orig_values"), 
                                      to = edu4_xwalk %>% purrr::pluck("xwalk") %>% purrr::pluck("edu4"), 
                                      warn_missing = F) %>% 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("a1_edu4", ref_df = reference_df),
                    a1_married = (CQFA001=="Married") %>% 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("a1_married", ref_df = reference_df), 
                    a1_menthealth = CQFB002 %>%                       zap_attributes() %>% 
                      as.numeric() %>% 
                      plyr::mapvalues(from = seq(5,1,by = -1), to = seq(1,5,by = 1)) %>% 
                      label_it("a1_menthealth", ref_df = reference_df), 
                    a1_physhealth = NA %>% #Really doesn't look like this is coded consistently with 2022 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("a1_physhealth", ref_df = reference_df),
                    CQR008 = as.character(CQR008),
                    a1_parent = (startsWith(CQR008,"Biological") | startsWith(CQR008,"Foster") | startsWith(CQR008,"Step")) %>% 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("a1_parent", ref_df = reference_df), 
                    a2_age = NA %>% 
                      as.numeric() %>% 
                      label_it("a2_age", ref_df = reference_df), 
                    a2_female = NA %>% 
                      as.numeric() %>% 
                      label_it("a2_female", ref_df = reference_df), 
                    a2_edu4 = NA %>% 
                      as.numeric() %>% 
                      label_it("a2_edu4", ref_df = reference_df), 
                    a2_married = NA %>% 
                      as.numeric() %>% 
                      label_it("a2_married", ref_df = reference_df), 
                    a2_menthealth = NA %>% 
                      as.numeric() %>% 
                      label_it("a2_menthealth", ref_df = reference_df), 
                    a2_physhealth = NA %>% 
                      as.numeric() %>% 
                      label_it("a2_physhealth", ref_df = reference_df), 
                    a2_parent = NA %>% 
                      as.numeric() %>% 
                      label_it("a2_parent", ref_df = reference_df), 
                    female = (as.character(CQR009)=="Femals") %>% 
                      zap_attributes() %>%
                      as.numeric() %>% 
                      label_it("female",ref_df=reference_df), 
                    hisp = startsWith(as.character(CQR011),"Yes") %>% 
                      zap_attributes() %>% 
                      as.numeric() %>% 
                      label_it("hisp", ref_df = reference_df), 
                    white_only = (CQR010=="White") & hisp==0, 
                    black_only = (CQR010=="Black or African American"), 
                    race4_char = ifelse(hisp==1, "Hispanic",
                                   ifelse(white_only, "White, non-Hispanic",
                                        ifelse(black_only, "Black, non-Hispanic", "Other/Multi-racial, non-Hispanic")
                                  )
                                 ) %>% zap_attributes() %>% as.character() %>% 
                                 label_it("race4_char", ref_df = reference_df), 
                    black_any = stringr::str_detect(CQR010,"Black") %>%
                      as.integer() %>% 
                      zap_attributes() %>% 
                      label_it("black_any", ref_df=reference_df),
                    acedivorce_rc = (CQR017=="Yes") %>% 
                      zap_attributes()  %>% 
                      as.numeric() %>% 
                      label_it("acedivorce_rc", ref_df = reference_df), 
                    acedeath_rc = (CQR018=="Yes") %>% 
                      zap_attributes()  %>% 
                      as.numeric() %>% 
                      label_it("acedeath_rc", ref_df = reference_df),
                    acejail_rc = (CQR019=="Yes") %>% 
                      zap_attributes()  %>% 
                      as.numeric() %>% 
                      label_it("acejail_rc", ref_df = reference_df), 
                    acedv_rc = (CQR020=="Yes") %>% 
                      zap_attributes()  %>% 
                      as.numeric() %>% 
                      label_it("acedv_rc", ref_df = reference_df), 
                    aceviolence_rc = (CQR021=="Yes") %>% 
                      zap_attributes()  %>% 
                      as.numeric() %>% 
                      label_it("aceviolence_rc", ref_df = reference_df), 
                    acementalill_rc = (CQR022=="Yes") %>% 
                      zap_attributes()  %>% 
                      as.numeric() %>% 
                      label_it("acementalill_rc", ref_df = reference_df), 
                    aceaddict_rc = (CQR023=="Yes") %>% 
                      zap_attributes()  %>% 
                      as.numeric() %>% 
                      label_it("aceaddict_rc", ref_df = reference_df), 
                    aceracism_rc =  (CQR024=="Yes") %>% #I have no idea the inconsistent coding in qualtrics 
                      zap_attributes()  %>% 
                      as.numeric() %>% 
                      label_it("aceracism_rc", ref_df = reference_df),
        chhealth = abs(as.integer(CQFA002)-6) %>% 
          zap_attributes() %>% 
          as.numeric() %>% 
          label_it("chhealth", ref_df = reference_df), 
        care10hrs_rc = (CQFB007=="Yes") %>% 
          zap_attributes() %>%  
          as.numeric() %>% 
          label_it("care10hrs_rc", ref_df = reference_df), 
        famcount = NA %>% 
          zap_attributes() %>% 
          as.numeric() %>% 
          label_it("famcount", ref_df = reference_df),
        cpi99 = cpi99 %>% 
          zap_attributes() %>% 
          as.numeric() %>% 
          label_it("cpi99", ref_df = reference_df), 
        CQR006 = CQR006 %>% 
          zap_attributes() %>% 
          as.numeric() %>% 
          round(0) %>% 
          as.integer(),
        inc99 = as.double(CQR006)*cpi99 %>% 
          zap_attributes() %>% 
          as.numeric() %>% 
          label_it("inc99", ref_df = reference_df),
        zip = zip %>% 
          zap_attributes() %>% 
          as.character() %>% 
          label_it("zip",ref_df = reference_df), 
        puma = puma %>% 
          zap_attributes() %>% 
          as.numeric() %>% 
          as.character() %>% 
          label_it("puma", ref_df = reference_df),
        rural = 0, 
        rural = rural %>% 
          zap_attributes() %>% 
          as.numeric() %>% 
          label_it("rural", ref_df = reference_df), 
        pctmetro = NA %>% 
          as.numeric() %>% 
          label_it("pctmetro", ref = reference_df) 
        )
    recoded_df$inc99[recoded_df$inc99<0] = 0
    
    return(recoded_df)


}
