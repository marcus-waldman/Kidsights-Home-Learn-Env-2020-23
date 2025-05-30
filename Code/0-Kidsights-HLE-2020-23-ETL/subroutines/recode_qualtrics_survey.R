recode_qualtrics_survey<-function(survey_df,reference_df, zipcodeR_tigris, cpi99=get_cpi99(2021)) {
  
  require(tidyverse)
  require(zipcodeR)
  
  edu4_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(survey_df$CQR004),
        grade = sjlabelled::get_labels(survey_df$CQR004, values = "n") %>% names() %>%  as.numeric(), 
        edu4 = c(0,0,1,2,2,2,3,3,3)
      ), 
      val_labs = c("Less than HS Diploma/GED"=0,"HS Diploma/GED"=1,"Some College, AA/AS, or Vocational/Trade/etc."=2,"BA/BS or Higher"=3), 
      var_lab = "Educational attainment (4 cat.; derived)"
    )
  
  
  recoded_df = survey_df %>% 
    dplyr::mutate(study = "UNMC22" %>% label_it("study", ref_df = reference_df), 
                  year = 2022 %>% label_it("year", ref_df = reference_df),  
                  fipsst = 31 %>% label_it("fipsst", ref_df = reference_df), 
                  uid = paste(study,year,fipsst,IPAddress,ResponseId,sep = "-") %>% label_it("uid", ref_df = reference_df), 
                  wgt = NA %>% as.numeric() %>% label_it("wgt", ref_df = reference_df), 
                  hhid = paste(study,year,fipsst,IPAddress, sep = "-") %>% label_it("hhid", ref_df = reference_df), 
                  famid = NA %>% as.character() %>% label_it("famid", ref_df = reference_df), 
                  zip = SQ001 %>% zipcodeR::normalize_zip() %>% zap_attributes(),
                  fwc = NA %>% as.numeric() %>% label_it("fwc", ref_df = reference_df), 
                  perwt = NA %>% as.numeric() %>% label_it("perwt", ref_df = reference_df), 
                  fpl_i1 = NA %>% as.numeric() %>% label_it("fpl_i1", ref_df = reference_df), 
                  foodstamp_rc = ifelse(is.na(CQR007_5),0,1) %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("foodstamp_rc", ref_df = reference_df),
                  medicare_rc = NA %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("medicare_rc", ref_df = reference_df),
                  govhealthc_rc = ifelse(is.na(CQR007_1), 0, 1) %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("govhealthc_rc", ref_df = reference_df),
                  wic_rc = ifelse(is.na(CQR007_7), 0, 1) %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("wic_rc", ref_df = reference_df),
                  cashass_rc = ifelse(is.na(CQR007_3), 0, 1) %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("cashass_rc", ref_df = reference_df),
                  mealfree_rc = ifelse(is.na(CQR007_4), 0, 1) %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("mealfree_rc", ref_df = reference_df),
                  a1_age = CQR003 %>% 
                    plyr::mapvalues(from = CQR003 %>%  sjlabelled::get_labels(values = "n") %>% names() %>% as.numeric(), 
                                    to = CQR003 %>% sjlabelled::get_labels() %>% as.numeric(), 
                                    warn_missing = F) %>% 
                    ifelse(.>=75,75,.) %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("a1_age", ref_df = reference_df), 
                  a1_female = (CQR002==2) %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("a1_female", ref_df = reference_df), 
                  a1_edu4 = CQR004 %>% 
                    plyr::mapvalues(from = edu4_xwalk %>% purrr::pluck("xwalk") %>% purrr::pluck("grade"), 
                                    to = edu4_xwalk %>% purrr::pluck("xwalk") %>% purrr::pluck("edu4"), 
                                    warn_missing = F) %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("a1_edu4", ref_df = reference_df), 
                  a1_married = (CQFA001==1) %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("a1_married", ref_df = reference_df), 
                  a1_menthealth = CQFB002 %>% #Amazingingly consistently coded 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("a1_menthealth", ref_df = reference_df), 
                  a1_physhealth = CFQB001 %>% #Amazingingly consistently coded 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("a1_physhealth", ref_df = reference_df), 
                  a1_parent = (CQR008==1 | CQR008==2 | CQR008==4) %>% 
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
                  startdate = StartDate %>% as.character() %>% as.Date(),#,
                  dob_month = EQ004a_1 %>% as.character() %>% 
                    plyr::mapvalues(from = EQ004a_1 %>%  sjlabelled::get_labels(values = "n") %>% names(), 
                                    to = EQ004a_1 %>% sjlabelled::get_labels(),
                                    warn_missing = F),
                  dob_day = EQ004a_2 %>% 
                    plyr::mapvalues(from = EQ004a_2 %>%  sjlabelled::get_labels(values = "n") %>% names() %>% as.numeric(), 
                                    to = EQ004a_2 %>% sjlabelled::get_labels() %>% as.numeric(),
                                    warn_missing = F) %>% 
                    zap_attributes(),
                  dob_year =EQ004a_3 %>% 
                    plyr::mapvalues(from = EQ004a_3 %>%  sjlabelled::get_labels(values = "n") %>% names() %>% as.numeric(), 
                                    to = EQ004a_3 %>% sjlabelled::get_labels() %>% as.numeric(),
                                    warn_missing = F) %>% 
                    zap_attributes(),
                  dob = paste0(dob_month," ", dob_day, ", ", dob_year) %>% as.Date(format = "%B %d, %Y"), 
                  days = (startdate-dob) %>% as.integer(), 
                  sc_age_years = floor(days/365.25) %>% as.numeric() %>% zap_attributes() %>% label_it("sc_age_years",ref_df = reference_df), 
                  female = (CQR009==2) %>% 
                    zap_attributes() %>%
                    as.numeric() %>% 
                    label_it("female",ref_df=reference_df), 
                  hisp = (CQR011==2 | CQR011==3 |CQR011==4 | CQR011==5) %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("hisp", ref_df = reference_df)
    ) 
  
  tmp = recoded_df %>% 
    dplyr::select(starts_with("CQR010")) %>% 
    apply(1,"sum",na.rm = T)
  
  recoded_df = recoded_df %>% 
    transform(sum_race = tmp) %>% 
    mutate(white_only = (sum_race==1 & !is.na(CQR010_1) & hisp==0), 
           black_only = (sum_race==1 & !is.na(CQR010_2) & hisp==0)
    ) %>% 
    mutate(race4_char = ifelse(hisp==1, "Hispanic",
                               ifelse(white_only, "White, non-Hispanic",
                                      ifelse(black_only, "Black, non-Hispanic", "Other/Multi-racial, non-Hispanic")
                               )
    ) %>% zap_attributes() %>% as.character() %>% 
      label_it("race4_char", ref_df = reference_df), 
    black_any = as.numeric(!is.na(CQR010_2)) %>% zap_attributes() %>% label_it("black_any", ref_df=reference_df),
    acedivorce_rc = (CQR017==4) %>% 
      zap_attributes()  %>% 
      as.numeric() %>% 
      label_it("acedivorce_rc", ref_df = reference_df), 
    acedeath_rc = (CQR018==4) %>% 
      zap_attributes()  %>% 
      as.numeric() %>% 
      label_it("acedeath_rc", ref_df = reference_df),
    acejail_rc = (CQR019==4) %>% 
      zap_attributes()  %>% 
      as.numeric() %>% 
      label_it("acejail_rc", ref_df = reference_df), 
    acedv_rc = (CQR020==4) %>% 
      zap_attributes()  %>% 
      as.numeric() %>% 
      label_it("acedv_rc", ref_df = reference_df), 
    aceviolence_rc = (CQR021==4) %>% 
      zap_attributes()  %>% 
      as.numeric() %>% 
      label_it("aceviolence_rc", ref_df = reference_df), 
    acementalill_rc = (CQR022==4) %>% 
      zap_attributes()  %>% 
      as.numeric() %>% 
      label_it("acementalill_rc", ref_df = reference_df), 
    aceaddict_rc = (CQR023==4) %>% 
      zap_attributes()  %>% 
      as.numeric() %>% 
      label_it("aceaddict_rc", ref_df = reference_df), 
    aceracism_rc =  (CQR024==1) %>% #I have no idea the inconsistent coding in qualtrics 
      zap_attributes()  %>% 
      as.numeric() %>% 
      label_it("aceracism_rc", ref_df = reference_df), 
    chhealth = CQFA002 %>% #Consistently coded. 
      zap_attributes() %>% 
      as.numeric() %>% 
      label_it("chhealth", ref_df = reference_df), 
    care10hrs_rc = CQFB007 %>% 
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
      stringr::str_remove_all("\\$") %>% 
      stringr::str_remove_all(",") %>% 
      as.numeric() %>% 
      round(0) %>% 
      as.integer(),
    inc99 = CQR006*cpi99 %>% 
      zap_attributes() %>% 
      as.numeric() %>% 
      label_it("inc99", ref_df = reference_df)
    )
  recoded_df$inc99[recoded_df$inc99<0] = 0
  
  
  # Bring in geographic information from zipcodeR and tigris package
  n1 = nrow(recoded_df)
  recoded_df = recoded_df %>% 
    left_join(zipcodeR_tigris %>% dplyr::mutate(zip = zipcode, puma = tigris_puma, rural = as.integer(!tigris_cbsa)) %>% dplyr::select(zip,puma, rural), 
              by = "zip")
  n2 = nrow(recoded_df)
  if(n1!=n2){error("problem joining recoded data frame with data frame zipcodeR_tigris informaiton.")}
  
  recoded_df  = recoded_df %>% 
    dplyr::mutate(puma = puma %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    as.character() %>% 
                    label_it("puma", ref_df = reference_df), 
                  rural = rural %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("rural", ref_df = reference_df), 
                  pctmetro = NA %>% as.numeric() %>% label_it("pctmetro", ref = reference_df) 
                  )
  
  return(recoded_df)
  
  
}

