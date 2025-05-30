recode_acs<-function(path, raw_acs, 
                     ref_df=readr::read_rds(file = paste0(path, "/data-files/intermediate-files/cahmi_2016_2020.RDS")) %>% 
                            recode_cahmi(path = path, cahmi_2016_2020 = .), 
                     inclusive = F){

  library(tidyverse)
  

  EDUCD_2_grade=readxl::read_excel(path=paste0(path,"/data-files/intermediate-files/crosswalks_acs2cahmi.xlsx"), sheet = "EDUCD2grade")
  names(EDUCD_2_grade) = c("EDUCD","label","grade","edu6", "edu4","notes")
  MARST_2_marital4=readxl::read_excel(path=paste0(path,"/data-files/intermediate-files/crosswalks_acs2cahmi.xlsx"), sheet = "MARST2marital4")
  
  
  raw_acs =  raw_acs %>% 
    dplyr::mutate(study = "ACS5-2020" %>% 
                    label_it("study", ref_df = ref_df), 
                  year = MULTYEAR %>% 
                    ipumsr::zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("year", ref_df = ref_df),
                  fipsst = STATEFIP %>% 
                    ipumsr::zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("fipsst", ref_df = ref_df), 
                  hhid = paste(year,fipsst,SERIAL, sep = "-") %>% 
                    label_it("hhid", ref_df = ref_df), 
                  famid = paste(hhid,FAMUNIT,sep = "-") %>% 
                    label_it("famid", ref_df = ref_df), 
                  uid = paste(famid,PERNUM, sep = "-") %>% 
                    label_it("uid", ref_df = ref_df)
  )
  
  
  famcount_df = raw_acs %>% dplyr::group_by(famid) %>% dplyr::summarize(famcount = length(uid))
  
  nrow(raw_acs)
  raw_acs = raw_acs %>% 
    dplyr::left_join(famcount_df, by = "famid") %>% 
    mutate(famcount = famcount %>% 
             zap_attributes() %>% 
             as.numeric() %>% 
             label_it("famcount", ref_df = ref_df))
  nrow(raw_acs)
  
  raw_acs = raw_acs %>% mutate(
                  puma = PUMA %>% 
                    ipumsr::zap_ipums_attributes() %>% 
                    as.character() %>% 
                    label_it("puma", ref_df = ref_df), 
                  pctmetro = PCTMETRO %>% 
                    ipumsr::zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("pctmetro", ref_df = ref_df), 
                  rural = ifelse(pctmetro>0 & pctmetro<100, NA, (100-pctmetro)/100) %>% 
                    zap_attributes() %>% 
                    as.integer() %>% 
                    label_it("rural", ref_df = ref_df), 
                  fwc = NA %>% 
                    as.numeric() %>% 
                    label_it("fwc", ref_df = ref_df), 
                  perwt = PERWT %>% 
                    ipumsr::zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("perwt", ref_df = ref_df), 
                  fpl_i1 = ifelse(POVERTY<50, 50, ifelse(POVERTY>400, 400, POVERTY)) %>% 
                    ipumsr::zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("fpl_i1", ref_df = ref_df), 
                  foodstamp_rc = plyr::mapvalues(FOODSTMP, from = c(0,1,2), to = c(NA,0,1), warn_missing = F) %>% 
                    ipumsr::zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("foodstamp_rc", ref_df = ref_df), 
                  a1_age = ifelse(AGE>=75, 75, ifelse(AGE>=18,AGE, NA)) %>% 
                    ipumsr::zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("a1_age", ref_df = ref_df), 
                  a1_female = ifelse(AGE>=18, SEX==2, NA) %>% 
                    ipumsr::zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("a1_female", ref_df = ref_df), 
                  a1_edu4 = ifelse(AGE>=18, plyr::mapvalues(EDUCD, from = EDUCD_2_grade$EDUCD, to = EDUCD_2_grade$edu4, warn_missing = F), NA) %>% 
                    ipumsr::zap_ipums_attributes() %>% 
                    as.numeric() %>%
                    label_it("a1_edu4", ref_df = ref_df), 
                  a1_married = ifelse(AGE>=18, plyr::mapvalues(MARST, from = MARST_2_marital4$MARST, to = MARST_2_marital4$married, warn_missing = F), NA) %>% 
                    ipumsr::zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("a1_married", ref_df = ref_df), 
                  a1_menthealth = NA %>% 
                    as.numeric() %>% 
                    label_it("a1_menthealth", ref_df = ref_df),
                  a1_physhealth =  NA %>% 
                    as.numeric() %>% 
                    label_it("a1_physhealth", ref_df = ref_df),
                  a1_parent = NA %>% 
                    as.numeric() %>% 
                    label_it("a1_parent", ref_df = ref_df),
                  a2_age =  NA %>% 
                    as.numeric() %>% 
                    label_it("a2_age", ref_df = ref_df),
                  a2_female=  NA %>% 
                    as.numeric() %>% 
                    label_it("a2_female", ref_df = ref_df),
                  a2_edu4=  NA %>% 
                    as.numeric() %>% 
                    label_it("a2_edu4", ref_df = ref_df),
                  a2_married=  NA %>% 
                    as.numeric() %>% 
                    label_it("a2_married", ref_df = ref_df),
                  a2_menthealth=  NA %>% 
                    as.numeric() %>% 
                    label_it("a2_menthealth", ref_df = ref_df),
                  a2_physhealth=  NA %>% 
                    as.numeric() %>% 
                    label_it("a2_physhealth", ref_df = ref_df),
                  a2_parent=  NA %>% 
                    as.numeric() %>% 
                    label_it("a2_parent", ref_df = ref_df),
                  sc_age_years = ifelse(AGE<18,AGE,NA) %>% 
                    zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("sc_age_years", ref_df = ref_df), 
                  female = ifelse(AGE<18, SEX==2, NA) %>% 
                    zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("female", ref_df = ref_df), 
                  race4_char = ifelse(AGE<18, 
                                        ifelse(HISPAN==1|HISPAN==2|HISPAN==3|HISPAN==4,"Hispanic",
                                          ifelse(HISPAN==0 & RACE==1, "White, non-Hispanic",
                                                ifelse(HISPAN==0 & RACE==2, "Black, non-Hispanic",
                                                        ifelse(HISPAN==0 & RACE>=3 & RACE<=9, "Other/Multi-racial, non-Hispanic", as.character(NA))
                                                ) # WHEN MERGING CHECK THAT CHARACTERS ARE UNIQUE ACROSS DATASET
                                          )
                                        ), 
                                      as.character(NA)
                                      ) %>% 
                    label_it("race4_char", ref_df = ref_df), 
                  RACED_char = RACED %>% 
                    zap_ipums_attributes() %>% 
                    plyr::mapvalues(from = ipumsr::ipums_val_labels(raw_acs, "RACED")$val, 
                                    to = ipumsr::ipums_val_labels(raw_acs, "RACED")$lbl, 
                                    warn_missing = F),
                  black_any = ifelse(AGE<18, stringr::str_detect(RACED_char, "Black") & RACED_char != "Blackfoot", NA) %>% 
                    zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("black_any", ref_df = ref_df), 
                  hisp = ifelse(AGE<18, HISPAN==1|HISPAN==2|HISPAN==3|HISPAN==4, NA) %>% 
                    zap_ipums_attributes() %>% 
                    as.numeric() %>% 
                    label_it("hisp", ref_df = ref_df),
                  acedivorce_rc = NA %>% 
                    as.numeric() %>% 
                    label_it("acedivorce_rc", ref_df = ref_df),
                  acedeath_rc = NA %>% 
                    as.numeric() %>% 
                    label_it("acedeath_rc", ref_df = ref_df), 
                  acejail_rc= NA %>% 
                    as.numeric() %>% 
                    label_it("acejail_rc", ref_df = ref_df),
                  acedv_rc= NA %>% 
                    as.numeric() %>% 
                    label_it("acedv_rc", ref_df = ref_df),
                  aceviolence_rc= NA %>% 
                    as.numeric() %>% 
                    label_it("aceviolence_rc", ref_df = ref_df),
                  acementalill_rc= NA %>% 
                    as.numeric() %>% 
                    label_it("acementalill_rc", ref_df = ref_df),
                  aceaddict_rc= NA %>% 
                    as.numeric() %>% 
                    label_it("aceaddict_rc", ref_df = ref_df),
                  aceracism_rc= NA %>% 
                    as.numeric() %>% 
                    label_it("aceracism_rc", ref_df = ref_df),
                  chhealth= NA %>% 
                    as.numeric() %>% 
                    label_it("chhealth", ref_df = ref_df),
                  care10hrs_rc= NA %>% 
                    as.numeric() %>% 
                    label_it("care10hrs_rc", ref_df = ref_df),
                  wgt = perwt/2 %>% 
                    zap_attributes() %>% 
                    label_it("wgt", ref_df = ref_df), 
                  cpi99 = CPI99 %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("cpi99", ref_df = ref_df), 
                  inc99 = ifelse(FTOTINC==9999999 | FTOTINC == 9999998, NA, 
                                 ifelse(FTOTINC <0, 0, FTOTINC*cpi99)) %>% 
                    zap_attributes() %>% 
                    as.numeric() %>% 
                    label_it("inc99", ref_df = ref_df)
                  
                  
    )


  raw_acs = raw_acs %>% 
    mutate(medicare_rc = plyr::mapvalues(HINSCARE, from = c(1,2), to = c(0,1), warn_missing = F) %>% 
           ipumsr::zap_ipums_attributes() %>% 
           as.numeric() %>% 
           label_it("medicare_rc", ref_df = ref_df)) %>% 
  mutate(govhealthc_rc = plyr::mapvalues(HCOVPUB, from = c(1,2), to = c(0,1), warn_missing = F) %>% 
           ipumsr::zap_ipums_attributes() %>% 
           as.numeric() %>% 
           label_it("govhealthc_rc", ref_df = ref_df)) %>% 
  mutate(wic_rc = NA %>% 
           ipumsr::zap_ipums_attributes() %>% 
           as.numeric() %>% 
           label_it("wic_rc", ref_df = ref_df)) %>% 
  mutate(INCWELFR_rc = ifelse(INCWELFR==99999, NA, INCWELFR)) %>% 
  mutate(cashass_rc = (INCWELFR_rc>0) %>% 
           ipumsr::zap_ipums_attributes() %>% 
           as.numeric() %>%
           label_it("cashass_rc", ref_df = ref_df) 
  ) %>% 
  mutate(mealfree_rc = NA %>% 
           ipumsr::zap_ipums_attributes() %>% 
           as.numeric() %>% 
           label_it("mealfree_rc", ref_df = ref_df))


  if(inclusive){
    acs_rc = raw_acs %>% 
      dplyr::select(PERNUM, MOMLOC, POPLOC, MOMLOC2, POPLOC2,all_of(names(ref_df)))
  } else {
    acs_rc = raw_acs %>% 
      dplyr::select(all_of(names(ref_df))) %>% 
      dplyr::filter(sc_age_years<18)
  }

  return(acs_rc)

}

