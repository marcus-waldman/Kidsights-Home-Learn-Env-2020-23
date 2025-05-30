# wide_df = just_screener_df
# reference_df = child_ne
# cpi99=get_cpi99(2021)

lengthen_recode_qualtrics_screener<-function(wide_df,reference_df, zipcodeR_tigris, cpi99=get_cpi99(2021)){
  
  require(tidyverse)
  require(zipcodeR)
  
  edu4_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(wide_df$CQR004),
        grade = sjlabelled::get_labels(wide_df$CQR004, values = "n") %>% names() %>%  as.numeric(), 
        edu4 = c(0,0,1,2,2,2,3,3,3)
      ), 
      val_labs = c("Less than HS Diploma/GED"=0,"HS Diploma/GED"=1,"Some College, AA/AS, or Vocational/Trade/etc."=2,"BA/BS or Higher"=3), 
      var_lab = "Educational attainment (4 cat.; derived)"
    )
  
  long_df = wide2long_screener(wide_df)
  
  long_df = long_df %>%
    dplyr::mutate(study = "screener-round0" %>% label_it("study", ref_df = reference_df),
                  year = 2022 %>% label_it("year", ref_df = reference_df),
                  fipsst = 31 %>% label_it("fipsst", ref_df = reference_df),
                  uid = uid %>% label_it("uid", ref_df = reference_df),
                  wgt = NA %>% as.numeric() %>% label_it("wgt", ref_df = reference_df),
                  hhid = NA %>% as.character() %>% label_it("hhid", ref_df = reference_df),
                  famid = paste0('scfamid-', ResponseId) %>% as.character() %>% label_it("famid", ref_df = reference_df),
                  zip = SQZIP %>% zipcodeR::normalize_zip() %>% zap_attributes() 
    )
    
  
  
  # Bring in geographic information from zipcodeR and tigris package
  n1 = nrow(long_df)
  long_df = long_df %>% 
    left_join(zipcodeR_tigris %>% dplyr::mutate(zip = zipcode, puma = tigris_puma, rural = as.integer(!tigris_cbsa)) %>% dplyr::select(zip,puma, rural), 
              by = "zip")
  n2 = nrow(long_df)
  if(n1!=n2){error("problem joining recoded data frame with data frame zipcodeR_tigris informaiton.")}
  
  
  long_df = long_df %>% 
    dplyr::mutate(puma = puma %>% zap_attributes() %>%  as.character() %>% label_it("puma", ref_df = reference_df),
                  rural = rural %>% zap_attributes() %>% label_it("rural", ref_df = reference_df),
                  fwc = NA ,
                  perwt = NA,
                  fpl_i1 = NA,
                  foodstamp_rc = NA,
                  a1_age = NA,
                  a1_female = NA,
                  a1_edu4 = SQCED %>%
                    plyr::mapvalues(from = edu4_xwalk %>% purrr::pluck("xwalk") %>% purrr::pluck("grade"),
                                    to = edu4_xwalk %>% purrr::pluck("xwalk") %>% purrr::pluck("edu4"),
                                    warn_missing = F) %>%
                    zap_attributes() %>%
                    as.numeric() %>%
                    label_it("a1_edu4", ref_df = reference_df),
                  a1_married = NA,
                  a1_menthealth = SQCMENH %>% #Amazingingly consistently coded
                    zap_attributes() %>%
                    as.numeric() %>%
                    label_it("a1_menthealth", ref_df = reference_df),
                  a1_physhealth = NA,
                  a1_parent = NA,
                  a2_age = NA,
                  a2_female = NA,
                  a2_edu4 = as.character(NA) %>%
                    plyr::mapvalues(from = edu4_xwalk %>% purrr::pluck("xwalk") %>% purrr::pluck("grade"),
                                    to = edu4_xwalk %>% purrr::pluck("xwalk") %>% purrr::pluck("edu4"),
                                    warn_missing = F) %>%
                    zap_attributes() %>%
                    as.numeric() %>%
                    label_it("a2_edu4", ref_df = reference_df),
                  a2_married = NA,
                  a2_menthealth = NA,
                  a2_physhealth = NA,
                  a2_parent = NA
    ) %>%
    mutate(startdate = StartDate %>% as.character() %>% as.Date(),
           EQ004a_1 = SQCBD_1,
           EQ004a_2 = SQCBD_2,
           EQ004a_3 = SQCBD_3,
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
           days = (startdate-dob) %>% as.integer()
    ) %>%
    mutate(sc_age_years = floor(days/365.25) %>% as.numeric() %>% zap_attributes() %>% label_it("sc_age_years",ref_df = reference_df),
           female = (SQSEX==2) %>%
             zap_attributes() %>%
             as.numeric() %>%
             label_it("female",ref_df=reference_df),
           hisp = as.integer(SQCETH>1) %>% 
             zap_attributes() %>% 
             as.numeric() %>% 
             label_it("hisp", ref_df = reference_df)
    )
  
  tmp = long_df %>% 
    dplyr::select(starts_with("SQRACE")) %>% 
    apply(1,"sum",na.rm = T)
  
  long_df = long_df %>% 
    transform(sum_race = tmp)
  
  long_df = long_df %>% 
    mutate(race4_char = NA %>% as.character()) %>%
    mutate(black_any = ifelse(is.na(SQRACE_2),0,1) %>%
             zap_attributes() %>%
             as.numeric() %>%
             label_it("black_any", ref_df = reference_df),
           acedivorce_rc = NA,
           acedeath_rc = NA,
           acejail_rc = NA,
           acedv_rc = NA,
           aceviolence_rc = NA,
           acementalill_rc = NA,
           aceaddict_rc = NA,
           aceracism_rc =  NA,
           chhealth = SQCHEALTH %>% #Consistently coded.
             zap_attributes() %>%
             as.numeric() %>%
             label_it("chhealth", ref_df = reference_df),
           care10hrs_rc = SQCARE %>%
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
           SQINC = SQINC %>% 
             stringr::str_remove_all("\\$") %>% 
             stringr::str_remove_all(",") %>% 
             as.numeric() %>% 
             round(0) %>% 
             as.integer(),
           inc99 = (SQINC*cpi99) %>%
             zap_attributes() %>%
             as.numeric() %>%
             label_it("inc99", ref_df = reference_df),
           medicare_rc = NA,
           govhealthc_rc = NA,
           wic_rc = NA,
           cashass_rc = NA,
           mealfree_rc = NA,
           white_any =  ifelse(is.na(SQRACE_2),0,1)
    )
  long_df$inc99[long_df$inc99<0]=0
  
  long_recoded_df = long_df
  
  
  
  
  return(long_recoded_df)
}