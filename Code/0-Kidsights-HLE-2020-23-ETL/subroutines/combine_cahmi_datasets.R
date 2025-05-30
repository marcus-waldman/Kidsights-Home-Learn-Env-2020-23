combine_cahmi_datasets<-function(path, save_out = F){
  
  # Load in the raw datasets
  
  cahmi_2019_2020 = haven::read_dta(paste0(path,"/data-files/source-files/CAHMI-2019-2020/raw-data/2019-2020_NSCH_Stata_CAHMI_DRC/2019-2020 NSCH_Topical_CAHMI_DRC.dta"))
  cahmi_2017_2018 = haven::read_dta(paste0(path,"/data-files/source-files/CAHMI-2017-2018/raw-data/2017-2018_NSCH_Topical_STATA_DRC_Jan2020/2017-2018 NSCH_topical_DRC_Dec 2019.dta"))
  cahmi_2016 = haven::read_dta(paste0(path,"/data-files/source-files/CAHMI-2016/raw-data/drc-2016-nsch-stata-v3/2016NSCHTopical_DRCv3__Stata13_Sep2018.dta"))
  
  
  ############### year, fipsst, stratum, hhid, fwc #####################
  
  cahmi_2016_2020 = list(extract_vars_cahmi(source="2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016), 
                         extract_vars_cahmi(source="2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016), 
                         extract_vars_cahmi(source="2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016)) %>% 
    dplyr::bind_rows() # Combined dataset
  
  # Check the weights are correct noting that under 18 pop is around 73 million in 2020 
  #sum(cahmi_2016_2020$fwc) # 73222987, correct
  
  
  ########  a1_grade: Caregiver 1 Educational Attainment #############
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_grade")
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_GRADE") %>% 
    mutate(a1_grade = ifelse(a1_grade>=90, NA,a1_grade))
  
  # 2016:
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_GRADE") %>% 
    mutate(a1_grade = ifelse(a1_grade>=90, NA,a1_grade))
  
  
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows() 
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>%
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  a2_grade: Caregiver 2 Educational Attainment #############
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_grade")
  #unique(tmp_1920$a2_grade)
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_GRADE") 
  #unique(tmp_1718$a2_grade)
  tmp_1718 = tmp_1718 %>% 
    mutate(a2_grade = ifelse(a2_grade>=90, NA,a2_grade))
  
  # 2016:
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_GRADE") 
  #unique(tmp_16$a2_grade)
  
  
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows() 
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>%
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  ###### adulteduc ##########
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "AdultEduc_1920") %>% 
    mutate(adulteduc = AdultEduc_1920) %>% dplyr::select(-AdultEduc_1920)
  unique(tmp_1920$adulteduc)
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "AdultEduc_1718") %>% 
    mutate(adulteduc = adulteduc_1718) %>% dplyr::select(-adulteduc_1718)
  unique(tmp_1718$adulteduc)
  
  # 2016:
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "AdultEduc_16") %>% 
    mutate(adulteduc = adulteduc_16) %>% dplyr::select(-adulteduc_16)
  unique(tmp_16$adulteduc)
  
  
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows() 
  tmp_16_20$adulteduc[tmp_16_20$adulteduc>=90] = NA
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  
  ############### Percent Above Poverty Line #####################
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "fpl_i1")
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FPL_I1") 
  
  # 2016:
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FPL")
  tmp_16 = tmp_16 %>% mutate(fpl_i1 = fpl) %>% 
    dplyr::select(-fpl)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows() 
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  WIC Benefits - Past 12 Months #############
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "WIC_1920") %>% 
    mutate(wic = WIC_1920) %>% 
    dplyr::select(-WIC_1920)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "WIC_1718") %>% 
    mutate(wic = wic_1718) %>% 
    dplyr::select(-wic_1718)
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "WIC_16") %>% 
    mutate(wic = wic_16) %>% 
    dplyr::select(-wic_16)
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  tmp_16_20 = tmp_16_20 %>% mutate(wic = ifelse(wic==99,NA,wic))
  
  tmp_16_20 = tmp_16_20 %>% 
    mutate(
      wic = zap_attributes(wic), 
      wic = haven::labelled(wic, 
                            labels = c(Yes=1, No=2), 
                            label = "Received benefits from the WIC Program at any time during the past 12 months")
    )
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  
  ########  Cash Assistance - Past 12 Months #############
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "CashAss_1920") %>% 
    mutate(cashass = CashAss_1920) %>% 
    dplyr::select(-CashAss_1920)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "CashAss_1718") %>% 
    mutate(cashass = cashass_1718) %>% 
    dplyr::select(-cashass_1718)
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "CashAss_16") %>% 
    mutate(cashass = cashass_16) %>% 
    dplyr::select(-cashass_16)
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  tmp_16_20 = tmp_16_20 %>% mutate(cashass = ifelse(cashass==99,NA,cashass))
  
  tmp_16_20 = tmp_16_20 %>% 
    mutate(cashass = zap_attributes(cashass),
           cashass = haven::labelled(cashass, 
                                     labels = c(Yes=1,No = 2), 
                                     label = "Received cash assistance from government at any time during the past 12 months")
    )
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  Food Stamp or SNAP - Past 12 Months #############
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FoodStamp_1920") %>% 
    mutate(foodstamp = FoodStamp_1920) %>% 
    dplyr::select(-FoodStamp_1920)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FoodStamp_1718") %>% 
    mutate(foodstamp = foodstamp_1718) %>% 
    dplyr::select(-foodstamp_1718)
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FoodStamp_16") %>% 
    mutate(foodstamp = foodstamp_16) %>% 
    dplyr::select(-foodstamp_16)
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  tmp_16_20 = tmp_16_20 %>% mutate(foodstamp = ifelse(foodstamp==99,NA,foodstamp))
  
  tmp_16_20 = tmp_16_20 %>% 
    mutate(foodstamp = zap_attributes(foodstamp),
           foodstamp = haven::labelled(foodstamp, 
                                       labels = c(Yes=1,No = 2), 
                                       label = "Received Food Stamps or Supplemental Nutrition Assistance Program benefits")
    )
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  Free or Reduced Price Breakfast or Lunch - Past 12 Months #############
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "MealFree_1920") %>% 
    mutate(mealfree = MealFree_1920) %>% 
    dplyr::select(-MealFree_1920)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "MealFree_1718") %>% 
    mutate(mealfree = mealfree_1718) %>% 
    dplyr::select(-mealfree_1718)
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "MealFree_16") %>% 
    mutate(mealfree = mealfree_16) %>% 
    dplyr::select(-mealfree_16)
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  tmp_16_20 = tmp_16_20 %>% mutate(mealfree = ifelse(mealfree==99,NA,mealfree))
  
  
  tmp_16_20 = tmp_16_20 %>% 
    mutate(mealfree = zap_attributes(mealfree),
           mealfree = haven::labelled(mealfree, 
                                      labels = c(Yes=1,No = 2), 
                                      label = "Received free or reduced-cost breakfasts or lunches at school")
    )
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  ######## Public or government provided healthcare #####
  cahmi_2016$K12Q12 %>% unique()
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "k12q12") %>% 
    mutate(govhealthc = k12q12) %>% 
    dplyr::select(-k12q12)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "K12Q12") %>% 
    mutate(govhealthc = k12q12) %>% 
    dplyr::select(-k12q12)
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "K12Q12") %>% 
    mutate(govhealthc = k12q12) %>% 
    dplyr::select(-k12q12)
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  tmp_16_20 = tmp_16_20 %>% mutate(govhealthc = ifelse(govhealthc>2,NA,govhealthc))
  
  
  tmp_16_20 = tmp_16_20 %>% 
    mutate(govhealthc = zap_attributes(govhealthc),
           govhealthc = haven::labelled(govhealthc, 
                                        labels = c(Yes=1,No = 2), 
                                        label = "Government assisted health care")
    )
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  
  
  ########  foodcash (Derived): Indicator 6.27: Received food or cash assistance during the past 12 months #############
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    mutate(foodcash= as.integer((wic==1) + (cashass==1) + (foodstamp==1) + (mealfree==1))) %>% 
    mutate(foodcash = plyr::mapvalues(foodcash,from = c(0,1,2,3,4), to = c(3,2,2,1,1))) %>% 
    mutate(foodcash = zap_attributes(foodcash),
           foodcash = haven::labelled(foodcash, 
                                      labels= c("3-4 types"=1,"1-2 types"= 2, "None"=3), 
                                      label = "Received free or reduced-cost breakfasts or lunches at school")
    )
  
  
  ########  Adult 1 Relationship to Child #############
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_relation") 
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_RELATION") %>% 
    mutate(a1_relation = ifelse(a1_relation==5, 6, a1_relation) ) # THIS CATEGORY APPEARS IN THE DATA SET BUT NOT IN ANY VARIABLE LABELS, MUST BE CAHMI MISTAKE. Collapse `5 - Aunt or Uncle` with `6 - Other: Relative` becasue Aunt or Uncle not a category in this year
  
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_RELATION") %>% 
    mutate(a1_relation = ifelse(a1_relation==5, 6, a1_relation) ) # Collapse `5 - Aunt or Uncle` with `6 - Other: Relative` becasue Aunt or Uncle not a category in this year
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  
  
  tmp_16_20 = tmp_16_20 %>% 
    mutate(a1_relation = ifelse(a1_relation==99,NA,a1_relation))  %>% 
    mutate(a1_relation = zap_attributes(a1_relation) %>% 
             haven::labelled(labels = c("Biological or Adoptive Parent"=1, "Step-parent"=2, "Grandparent"=3, "Foster Parent"=4, "Other: Relative"=6, "Other: Non-Relative"=7), 
                             label = "Adult 1 - How Related to Child")
    )
  
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  Adult 2 Relationship to Child #############
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_relation") 
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_RELATION") %>% 
    mutate(a2_relation = ifelse(a2_relation==5, 6, a2_relation) )
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_RELATION") %>% 
    mutate(a2_relation = ifelse(a2_relation==5, 6, a2_relation) )
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  
  tmp_16_20 = tmp_16_20 %>% 
    mutate(a2_relation = ifelse(a2_relation==99 | a2_relation==8,NA,a2_relation))  %>% #Note that 8 is the child only has 1 caregiver. Set to missing
    mutate(a2_relation = zap_attributes(a2_relation) %>% 
             haven::labelled(labels = c("Biological or Adoptive Parent"=1, "Step-parent"=2, "Grandparent"=3, "Foster Parent"=4, "Other: Relative"=6, "Other: Non-Relative"=7), 
                             label = "Adult 2 - How Related to Child")
    )
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  sex: What is this child's sex #############
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sex_1920") %>% 
    mutate(sex = sex_1920) %>% dplyr::select(-sex_1920)
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sex_1718") %>% 
    mutate(sex = sex_1718) %>% dplyr::select(-sex_1718)
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sex_16") %>% 
    mutate(sex = sex_16) %>% dplyr::select(-sex_16)
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  tmp_16_20 = tmp_16_20 %>% mutate(sex = ifelse(sex==99,NA,sex))
  
  
  tmp_16_20 = tmp_16_20 %>% 
    mutate(sex = sex %>% zap_attributes() %>% 
             haven::labelled(labels= c("Male"=1,"Female" = 2), label = "Child's sex")
    ) 
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  sc_hispanic_r: Hispanic Origin of Selected Child, Recode #############
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_hispanic_r") 
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_HISPANIC_R") %>% 
    mutate(sc_hispanic_r = ifelse(sc_hispanic_r>=90,NA,sc_hispanic_r))
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_HISPANIC_R") 
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  
  tmp_16_20 = tmp_16_20 %>% 
    mutate(sc_hispanic_r = 
             sc_hispanic_r %>% zap_attributes() %>% 
             haven::labelled(labels = c("Hispanic or Latino Origin"=1, "Not Hispanic or Latino Origin"=2), 
                             label = "Hispanic Origin of Selected Child")
    )
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  ########  race4: Race and ethnicity distribution of the child population #############
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "race4_1920") %>% 
    mutate(race4 = race4_1920) %>% dplyr::select(-race4_1920)
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "race4_1718")  %>% 
    mutate(race4 = race4_1718) %>% dplyr::select(-race4_1718)
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "race4_16") %>% 
    mutate(race4 = race4_16) %>% dplyr::select(-race4_16)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  tmp_16_20$race4[tmp_16_20$race4==99]=NA
  
  tmp_16_20 = tmp_16_20 %>% 
    mutate(race4 = race4 %>% zap_attributes() %>% 
             haven::labelled(label = "Race and ethnicity distribution of the child population", 
                             labels = c("Hispanic"=1, "White, non-Hispanic"=2, "Black, non-Hispanic"=3, "Other/Multi-racial, non-Hispanic"=4))
    )
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  
  ########  rxmeds: Children qualifying on the CSHCN Screener prescription medication criteria #############
  unique(cahmi_2019_2020$sc_k2q10)
  table(cahmi_2019_2020$sc_k2q10, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_k2q10") %>% mutate(scrxmeds = sc_k2q10) %>% dplyr::select(-sc_k2q10)
  unique(tmp_1920$scrxmeds)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q10")  %>%  mutate(scrxmeds = sc_k2q10) %>% dplyr::select(-sc_k2q10) 
  unique(tmp_1718$scrxmeds)
  tmp_1718$scrxmeds[tmp_1718$scrxmeds>=90] = NA
  
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q10")  %>%  mutate(scrxmeds = sc_k2q10) %>% dplyr::select(-sc_k2q10) 
  unique(tmp_16$scrxmeds)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  
  
  with(tmp_16_20, unique(scrxmeds))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(scrxmeds = scrxmeds %>% zap_attributes() %>% 
             haven::labelled(label = "SC Needs or Uses Medication Currently", 
                             labels = c("Yes"=1,"No"=2))
    )
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  ########  scserve:  SC Needs or Uses More Medical Care than Others#############
  unique(cahmi_2019_2020$sc_k2q13)
  table(cahmi_2019_2020$sc_k2q13, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_k2q13") %>% mutate(scserve = sc_k2q13) %>% dplyr::select(-sc_k2q13)
  unique(tmp_1920$scserve)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q13")  %>%  mutate(scserve = sc_k2q13) %>% dplyr::select(-sc_k2q13) 
  unique(tmp_1718$scserve)
  tmp_1718$scserve[tmp_1718$scserve>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q13")  %>%  mutate(scserve = sc_k2q13) %>% dplyr::select(-sc_k2q13) 
  unique(tmp_16$scserve)
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,scserve))
  
  with(tmp_16_20, unique(scserve))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(scserve = scserve %>% zap_attributes() %>% 
             haven::labelled(label = "SC Needs or Uses More Medical Care than Other", 
                             labels = c("Yes"=1,"No"=2))
    )
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  scfunc: SC Limited Ability  #############
  unique(cahmi_2019_2020$sc_k2q16)
  table(cahmi_2019_2020$sc_k2q16, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_k2q16") %>% mutate(scfunc = sc_k2q16) %>% dplyr::select(-sc_k2q16)
  unique(tmp_1920$scfunc)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q16")  %>%  mutate(scfunc = sc_k2q16) %>% dplyr::select(-sc_k2q16) 
  unique(tmp_1718$scfunc)
  tmp_1718$scfunc[tmp_1718$scfunc>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q16")  %>%  mutate(scfunc = sc_k2q16) %>% dplyr::select(-sc_k2q16) 
  unique(tmp_16$scfunc)
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,scfunc))
  
  
  with(tmp_16_20, unique(scfunc))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(scfunc = scfunc %>% zap_attributes() %>% 
             haven::labelled(label = "SC Limited Ability Currently", 
                             labels = c("Yes"=1,"No"=2))
    )
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  sctherapy:  #############
  unique(cahmi_2019_2020$sc_k2q19)
  table(cahmi_2019_2020$sc_k2q19, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_k2q19") %>% mutate(sctherapy = sc_k2q19) %>% dplyr::select(-sc_k2q19)
  unique(tmp_1920$sctherapy)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q19")  %>%  mutate(sctherapy = sc_k2q19) %>% dplyr::select(-sc_k2q19) 
  unique(tmp_1718$sctherapy)
  tmp_1718$sctherapy[tmp_1718$sctherapy>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q19")  %>%  mutate(sctherapy = sc_k2q19) %>% dplyr::select(-sc_k2q19) 
  unique(tmp_16$sctherapy)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,sctherapy))
  
  with(tmp_16_20, unique(sctherapy))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(sctherapy = sctherapy %>% zap_attributes() %>% 
             haven::labelled(label = "SC Special Therapy", 
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_20, unique(sctherapy))
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  scmhealth:  #############
  unique(cahmi_2019_2020$sc_k2q22)
  table(cahmi_2019_2020$sc_k2q22, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_k2q22") %>% mutate(scmhealth = sc_k2q22) %>% dplyr::select(-sc_k2q22)
  unique(tmp_1920$scmhealth)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q22")  %>%  mutate(scmhealth = sc_k2q22) %>% dplyr::select(-sc_k2q22) 
  unique(tmp_1718$scmhealth)
  tmp_1718$scmhealth[tmp_1718$scmhealth>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q22")  %>%  mutate(scmhealth = sc_k2q22) %>% dplyr::select(-sc_k2q22) 
  unique(tmp_16$scmhealth)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,scmhealth))
  
  
  with(tmp_16_20, unique(scmhealth))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(scmhealth = scmhealth %>% zap_attributes() %>% 
             haven::labelled(label = "SC Emotion Develop Behave Treatment", 
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_20, unique(scmhealth))
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  Indicator 1.11 Children with speical needs care #############
  unique(cahmi_2019_2020$CSHCN_1920)
  table(cahmi_2019_2020$CSHCN_1920, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "CSHCN_1920") %>% mutate(cshcn = CSHCN_1920) %>% dplyr::select(-CSHCN_1920)
  unique(tmp_1920$cshcn)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "CSHCN_1718") %>% mutate(cshcn = cshcn_1718) %>% dplyr::select(-cshcn_1718) 
  unique(tmp_1718$cshcn)
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "CSHCN_16")  %>%  mutate(cshcn = cshcn_16) %>% dplyr::select(-cshcn_16) 
  unique(tmp_16$cshcn)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,cshcn, useNA = "always"))
  
  
  with(tmp_16_20, unique(cshcn, useNA = "always"))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(cshcn = cshcn %>% zap_attributes() %>% 
             haven::labelled(label = "Indicator 1.11: Children with special health care needs", 
                             labels = c("Meet screening criteria"=1,"Do not meet screening criteria"=2))
    )
  with(tmp_16_20, unique(cshcn))
  with(tmp_16_20, table(year,cshcn, useNA = "always"))
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  acedivorce: Divorced or separated  #############
  unique(cahmi_2019_2020$ace3)
  table(cahmi_2019_2020$ace3, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace3") %>% mutate(acedivorce = ace3) %>% dplyr::select(-ace3)
  unique(tmp_1920$acedivorce)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE3")  %>%  mutate(acedivorce = ace3) %>% dplyr::select(-ace3) 
  unique(tmp_1718$acedivorce)
  tmp_1718$acedivorce[tmp_1718$acedivorce>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE3")  %>%  mutate(acedivorce = ace3) %>% dplyr::select(-ace3) 
  unique(tmp_16$acedivorce)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,acedivorce))
  
  with(tmp_16_20, unique(acedivorce))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(acedivorce = acedivorce %>% zap_attributes() %>% 
             haven::labelled(label = "Child Experienced - Parent or Guardian Divorced", 
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_20, unique(acedivorce))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  acedeath: Parent or guardian died  #############
  unique(cahmi_2019_2020$ace4)
  table(cahmi_2019_2020$ace4, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace4") %>% mutate(acedeath = ace4) %>% dplyr::select(-ace4)
  unique(tmp_1920$acedeath)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE4")  %>%  mutate(acedeath = ace4) %>% dplyr::select(-ace4) 
  unique(tmp_1718$acedeath)
  tmp_1718$acedeath[tmp_1718$acedeath>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE4")  %>%  mutate(acedeath = ace4) %>% dplyr::select(-ace4) 
  unique(tmp_16$acedeath)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,acedeath))
  
  
  with(tmp_16_20, unique(acedeath))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(acedeath = acedeath %>% zap_attributes() %>% 
             haven::labelled(label = "Child Experienced - Parent or Guardian Died", 
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_20, unique(acedeath))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  
  ########  acejail: Parent or guardian in jail  #############
  unique(cahmi_2019_2020$ace5)
  table(cahmi_2019_2020$ace5, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace5") %>% mutate(acejail = ace5) %>% dplyr::select(-ace5)
  unique(tmp_1920$acejail)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE5")  %>%  mutate(acejail = ace5) %>% dplyr::select(-ace5) 
  unique(tmp_1718$acejail)
  tmp_1718$acejail[tmp_1718$acejail>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE5")  %>%  mutate(acejail = ace5) %>% dplyr::select(-ace5) 
  unique(tmp_16$acejail)
  
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,acejail))
  
  with(tmp_16_20, unique(acejail))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(acejail = acejail %>% zap_attributes() %>% 
             haven::labelled(label = "Child Experienced - Parent or Guardian Time in Jail", 
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_20, unique(acejail))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  ########  acedv: Saw or herd parents or adults slap, hit, kick, punch one aother  #############
  unique(cahmi_2019_2020$ace6)
  table(cahmi_2019_2020$ace6, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace6") %>% mutate(acedv = ace6) %>% dplyr::select(-ace6)
  unique(tmp_1920$acedv)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE6")  %>%  mutate(acedv = ace6) %>% dplyr::select(-ace6) 
  unique(tmp_1718$acedv)
  tmp_1718$acedv[tmp_1718$acedv>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE6")  %>%  mutate(acedv = ace6) %>% dplyr::select(-ace6) 
  unique(tmp_16$acedv)
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,acedv))
  
  with(tmp_16_20, unique(acedv))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(acedv = acedv %>% zap_attributes() %>% 
             haven::labelled(label = "Child Experienced - Adults Slap, Hit, Kick, Punch Others", 
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_20, unique(acedv))
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  aceviolence: Was a victim or witnessed violence  #############
  unique(cahmi_2019_2020$ace7)
  table(cahmi_2019_2020$ace7, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace7") %>% mutate(aceviolence = ace7) %>% dplyr::select(-ace7)
  unique(tmp_1920$aceviolence)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE7")  %>%  mutate(aceviolence = ace7) %>% dplyr::select(-ace7) 
  unique(tmp_1718$aceviolence)
  tmp_1718$aceviolence[tmp_1718$aceviolence>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE7")  %>%  mutate(aceviolence = ace7) %>% dplyr::select(-ace7) 
  unique(tmp_16$aceviolence)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,aceviolence))
  
  
  with(tmp_16_20, unique(aceviolence))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(aceviolence = aceviolence %>% zap_attributes() %>% 
             haven::labelled(label = "Child Experienced - Victim of Violence", 
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_20, unique(aceviolence))
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  ########  acementalill: Lived with anyone mentally ill, suicidal, or severly depressed  #############
  unique(cahmi_2019_2020$ace8)
  table(cahmi_2019_2020$ace8, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace8") %>% mutate(acementalill = ace8) %>% dplyr::select(-ace8)
  unique(tmp_1920$acementalill)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE8")  %>%  mutate(acementalill = ace8) %>% dplyr::select(-ace8) 
  unique(tmp_1718$acementalill)
  tmp_1718$acementalill[tmp_1718$acementalill>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE8")  %>%  mutate(acementalill = ace8) %>% dplyr::select(-ace8) 
  unique(tmp_16$acementalill)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,acementalill))
  
  with(tmp_16_20, unique(acementalill))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(acementalill = acementalill %>% zap_attributes() %>% 
             haven::labelled(label = "Child Experienced - Lived with Mentally Ill", 
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_20, unique(acementalill))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  aceaddict: #############
  unique(cahmi_2019_2020$ace9)
  table(cahmi_2019_2020$ace9, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace9") %>% mutate(aceaddict = ace9) %>% dplyr::select(-ace9)
  unique(tmp_1920$aceaddict)
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE9")  %>%  mutate(aceaddict = ace9) %>% dplyr::select(-ace9) 
  unique(tmp_1718$aceaddict)
  tmp_1718$aceaddict[tmp_1718$aceaddict>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE9")  %>%  mutate(aceaddict = ace9) %>% dplyr::select(-ace9) 
  unique(tmp_16$aceaddict)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,aceaddict))
  
  with(tmp_16_20, unique(aceaddict))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(aceaddict = aceaddict %>% zap_attributes() %>% 
             haven::labelled(label = "Child Experienced - Lived with Person with Alcohol/Drug Problem", 
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_20, unique(aceaddict))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  ########  aceracism: Treated ounfailrly because of his or her race or ethnic group #############
  unique(cahmi_2019_2020$ace10)
  table(cahmi_2019_2020$ace10, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace10") %>% mutate(aceracism = ace10) %>% dplyr::select(-ace10)
  unique(tmp_1920$aceracism)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE10")  %>%  mutate(aceracism = ace10) %>% dplyr::select(-ace10) 
  unique(tmp_1718$aceracism)
  tmp_1718$aceracism[tmp_1718$aceracism>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE10")  %>%  mutate(aceracism = ace10) %>% dplyr::select(-ace10) 
  unique(tmp_16$aceracism)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,aceracism))
  
  with(tmp_16_20, unique(aceracism))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(aceracism = aceracism %>% zap_attributes() %>% 
             haven::labelled(label = "Child Experienced - Treated Unfairly Because of Race", 
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_20, unique(aceracism))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  
  
  
  ########  a1_sex:  #############
  unique(cahmi_2019_2020$a1_sex)
  table(cahmi_2019_2020$a1_sex, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_sex") 
  unique(tmp_1920$a1_sex)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_SEX")  
  unique(tmp_1718$a1_sex)
  tmp_1718$a1_sex[tmp_1718$a1_sex>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_SEX") 
  unique(tmp_16$a1_sex)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,a1_sex))
  
  with(tmp_16_20, unique(a1_sex))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(a1_sex = a1_sex %>% zap_attributes() %>% 
             haven::labelled(label = "Adult 1 - Sex", 
                             labels = c("Male"=1,"Female"=2))
    )
  with(tmp_16_20, unique(a1_sex))
  
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  a2_sex:  #############
  unique(cahmi_2019_2020$a2_sex)
  table(cahmi_2019_2020$a2_sex, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_sex") 
  unique(tmp_1920$a2_sex)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_SEX")  
  unique(tmp_1718$a2_sex)
  tmp_1718$a2_sex[tmp_1718$a2_sex>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_SEX") 
  unique(tmp_16$a2_sex)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,a2_sex))
  
  
  with(tmp_16_20, unique(a2_sex))
  tmp_16_20 = tmp_16_20 %>% 
    mutate(a2_sex = a2_sex %>% zap_attributes() %>% 
             haven::labelled(label = "Adult 2 - Sex", 
                             labels = c("Male"=1,"Female"=2))
    )
  with(tmp_16_20, unique(a2_sex))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  ########  a1_age:  #############
  table(cahmi_2019_2020$a1_age, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_age") 
  unique(tmp_1920$a1_age)
  max(tmp_1920$a1_age,na.rm=T)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_AGE")  
  unique(tmp_1718$a1_age)
  max(tmp_1718$a1_age,na.rm=T)
  tmp_1718$a1_age[tmp_1718$a1_age>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_AGE") 
  unique(tmp_16$a1_age)
  with(tmp_16, sort(unique(a1_age)))
  with(tmp_16, table(a1_age))
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,a1_age))
  
  with(tmp_16_20, unique(a1_age))
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(a1_age = a1_age %>% zap_attributes() %>% 
                    haven::labelled(labels = c("75 or older" = 75), 
                                    label = "Adult 1 - Age in Years"))
  with(tmp_16_20, unique(a1_age))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  a2_age:  #############
  table(cahmi_2019_2020$a2_age, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_age") 
  unique(tmp_1920$a2_age)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_AGE")  
  unique(tmp_1718$a2_age)
  tmp_1718$a2_age[tmp_1718$a2_age>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_AGE") 
  unique(tmp_16$a2_age)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,a2_age))
  
  with(tmp_16_20, unique(a2_age))
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(a2_age = a2_age %>% zap_attributes() %>% 
                    haven::labelled(labels = c("75 or older" = 75), 
                                    label = "Adult 2 - Age in Years"))
  with(tmp_16_20, unique(a2_age))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  a1_physhealth:  #############
  unique(cahmi_2019_2020$a1_physhealth)
  table(cahmi_2019_2020$a1_physhealth, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_physhealth") 
  unique(tmp_1920$a1_physhealth)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_PHYSHEALTH")  
  unique(tmp_1718$a1_physhealth)
  tmp_1718$a1_physhealth[tmp_1718$a1_physhealth>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_PHYSHEALTH") 
  unique(tmp_16$a1_physhealth)
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,a1_physhealth))
  
  
  unique(tmp_16_20$a1_physhealth)
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(a1_physhealth = a1_physhealth %>% zap_attributes() %>% 
                    haven::labelled(labels = c("Excellent"=1, "Very Good"=2, "Good"=3, "Fair"=4, "Poor"=5), 
                                    label = "Adult 1 - Physical Health"))
  with(tmp_16_20, unique(a1_physhealth))
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  a2_physhealth:  #############
  unique(cahmi_2019_2020$a2_physhealth)
  table(cahmi_2019_2020$a2_physhealth, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_physhealth") 
  unique(tmp_1920$a2_physhealth)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_PHYSHEALTH")  
  unique(tmp_1718$a2_physhealth)
  tmp_1718$a2_physhealth[tmp_1718$a2_physhealth>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_PHYSHEALTH") 
  unique(tmp_16$a2_physhealth)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,a2_physhealth))
  
  unique(tmp_16_20$a2_physhealth)
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(a2_physhealth = a2_physhealth %>% zap_attributes() %>% 
                    haven::labelled(labels = c("Excellent"=1, "Very Good"=2, "Good"=3, "Fair"=4, "Poor"=5), 
                                    label = "Adult 2 - Physical Health"))
  with(tmp_16_20, unique(a2_physhealth))
  
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  a1_menthealth:  #############
  unique(cahmi_2019_2020$a1_menthealth)
  table(cahmi_2019_2020$a1_menthealth, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_menthealth") 
  unique(tmp_1920$a1_menthealth)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_MENTHEALTH")  
  unique(tmp_1718$a1_menthealth)
  tmp_1718$a1_menthealth[tmp_1718$a1_menthealth>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_MENTHEALTH") 
  unique(tmp_16$a1_menthealth)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,a1_menthealth))
  
  
  unique(tmp_16_20$a1_menthealth)
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(a1_menthealth = a1_menthealth %>% zap_attributes() %>% 
                    haven::labelled(labels = c("Excellent"=1, "Very Good"=2, "Good"=3, "Fair"=4, "Poor"=5), 
                                    label = "Adult 1 - Mental Health"))
  with(tmp_16_20, unique(a1_menthealth))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  ########  a2_menthealth  #############
  unique(cahmi_2019_2020$a2_menthealth)
  table(cahmi_2019_2020$a2_menthealth, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_menthealth") 
  unique(tmp_1920$a2_menthealth)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_MENTHEALTH")  
  unique(tmp_1718$a2_menthealth)
  tmp_1718$a2_menthealth[tmp_1718$a2_menthealth>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_MENTHEALTH") 
  unique(tmp_16$a2_menthealth)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,a2_menthealth))
  
  
  unique(tmp_16_20$a2_menthealth)
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(a2_menthealth = a2_menthealth %>% zap_attributes() %>% 
                    haven::labelled(labels = c("Excellent"=1, "Very Good"=2, "Good"=3, "Fair"=4, "Poor"=5), 
                                    label = "Adult 1 - Mental Health"))
  with(tmp_16_20, unique(a2_menthealth))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  ########  a1_marital  #############
  unique(cahmi_2019_2020$a1_marital)
  table(cahmi_2019_2020$a1_marital, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_marital") 
  unique(tmp_1920$a1_marital)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_MARITAL")  
  unique(tmp_1718$a1_marital)
  tmp_1718$a1_marital[tmp_1718$a1_marital>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_MARITAL") 
  unique(tmp_16$a1_marital)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,a1_marital, useNA = "always"))
  
  with(tmp_16_20, unique(a1_marital))
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(a1_marital = a1_marital %>% zap_attributes() %>% 
                    haven::labelled(labels = c("Married"=1, "Not married, but living with a partner"=2, "Never Married"=3, "Divorced"=4, "Separated"=5, "Widowed"=6), 
                                    label = "Adult 1 - Marital Status"))
  with(tmp_16_20, unique(a1_marital))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  a2_marital  #############
  unique(cahmi_2019_2020$a2_marital)
  table(cahmi_2019_2020$a2_marital, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_marital") 
  unique(tmp_1920$a2_marital)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_MARITAL")  
  unique(tmp_1718$a2_marital)
  tmp_1718$a2_marital[tmp_1718$a2_marital>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_MARITAL") 
  unique(tmp_16$a2_marital)
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,a2_marital, useNA = "always"))
  
  with(tmp_16_20, unique(a2_marital))
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(a2_marital = a2_marital %>% zap_attributes() %>% 
                    haven::labelled(labels = c("Married"=1, "Not married, but living with a partner"=2, "Never Married"=3, "Divorced"=4, "Separated"=5, "Widowed"=6), 
                                    label = "Adult 2 - Marital Status"))
  with(tmp_16_20, unique(a2_marital))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)> 
    rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  sc_age_years: Child's age in years  #############
  unique(cahmi_2019_2020$sc_age_years)
  table(cahmi_2019_2020$sc_age_years, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_age_years") 
  unique(tmp_1920$sc_age_years)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_AGE_YEARS")  
  unique(tmp_1718$sc_age_years)
  tmp_1718$sc_age_years[tmp_1718$sc_age_years>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_AGE_YEARS") 
  unique(tmp_16$sc_age_years)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,sc_age_years))
  
  
  with(tmp_16_20, unique(sc_age_years))
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(sc_age_years = sc_age_years %>% zap_attributes() %>% 
                    haven::labelled(label = "Child's Age in Years"))
  with(tmp_16_20, unique(sc_age_years))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  ########  care10hrs: Indicator 6.21: Received care from others at least 10 hours/week, age 0-5  #############
  unique(cahmi_2019_2020$k6q20)
  table(cahmi_2019_2020$k6q20, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "k6q20") %>% 
    mutate(care10hrs = k6q20) %>% dplyr::select(-k6q20)
  unique(tmp_1920$care10hrs)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "K6Q20") %>% 
    mutate(care10hrs = k6q20) %>% dplyr::select(-k6q20)
  unique(tmp_1718$care10hrs)
  tmp_1718$care10hrs[tmp_1718$care10hrs>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "K6Q20") %>% 
    mutate(care10hrs = k6q20) %>% dplyr::select(-k6q20)
  unique(tmp_16$care10hrs)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,care10hrs))
  
  
  with(tmp_16_20, unique(care10hrs))
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(care10hrs = care10hrs %>% zap_attributes() %>% 
                    haven::labelled(labels = c("Yes"=1, "No"=2), 
                                    label = "Receive Care From Others at Least 10 Hours Per Week"))
  with(tmp_16_20, unique(care10hrs))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  with(cahmi_2016_2020, table(sc_age_years,care10hrs, useNA="always"))
  
  
  ########## chhealth: Child's General Health #####
  unique(cahmi_2019_2020$k2q01)
  table(cahmi_2019_2020$k2q01, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "k2q01") %>% 
    mutate(chhealth = k2q01) %>% dplyr::select(-k2q01)
  unique(tmp_1920$chhealth)
  
  
  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "K2Q01") %>% 
    mutate(chhealth = k2q01) %>% dplyr::select(-k2q01)
  unique(tmp_1718$chhealth)
  tmp_1718$chhealth[tmp_1718$chhealth>=90] = NA
  
  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "K2Q01") %>% 
    mutate(chhealth = k2q01) %>% dplyr::select(-k2q01)
  unique(tmp_16$chhealth)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,chhealth))
  
  
  with(tmp_16_20, unique(chhealth))
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(chhealth = chhealth %>% zap_attributes() %>% 
                    haven::labelled(labels = c("Excellent"=1, "Very Good"=2, "Good"=3, "Fair"=4, "Poor"=5), 
                                    label = "Child's General Health"))
  with(tmp_16_20, unique(chhealth))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  # ###### (Derived) scalewgt: Scaled child weight #####
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    mutate(log2s_fwc = log(fwc/mean(fwc),2)) %>% 
    mutate(log2s_fwc = log2s_fwc %>% 
             zap_attributes() %>% 
             haven::labelled(label = "Log_2 of 2016-2020 Normalized NSCH Interview Child Weights"))
  
  
  # ###### Metropolitan status #####
  unique(cahmi_2019_2020$metro_yn)
  table(cahmi_2019_2020$metro_yn, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "metro_yn") %>% 
    mutate(rural = as.integer(metro_yn==2)) %>% dplyr::select(-metro_yn)
  unique(tmp_1920$rural)
  
  
  #2017-2018
  unique(cahmi_2017_2018$METRO_YN, useNA = "always")
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "METRO_YN") %>% 
    mutate(rural = ifelse(metro_yn==98,NA, as.integer(metro_yn==2))) %>% dplyr::select(-metro_yn)
  unique(tmp_1718$rural)
  table(tmp_1718$rural, useNA = "always")
  
  
  # 2016
  unique(cahmi_2016$METRO_YN, useNA = "always")
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "METRO_YN") %>% 
    mutate(rural = as.integer(metro_yn==2)) %>% dplyr::select(-metro_yn)
  unique(tmp_16$rural)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,rural, useNA = "always"))
  
  
  with(tmp_16_20, unique(rural))
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(rural = rural %>% zap_attributes() %>% 
                    haven::labelled(labels = c("In Metropolitan Statistical Area" =0, "Not In Metropolitan Statistical Area "=1), 
                                    label = "Rural Status (derived; based on Metropolitan Statistical Area Status)"))
  with(tmp_16_20, unique(rural))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  # ###### Number of family members #####
  unique(cahmi_2019_2020$famcount)
  table(cahmi_2019_2020$famcount, useNA = "always")
  
  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "famcount")
  unique(tmp_1920$famcount)
  
  
  #2017-2018
  unique(cahmi_2017_2018$FAMCOUNT, useNA = "always")
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FAMCOUNT") 
  unique(tmp_1718$famcount)
  table(tmp_1718$famcount, useNA = "always")
  tmp_1718$famcount[tmp_1718$famcount>=90] = NA
  
  
  # 2016
  unique(cahmi_2016$FAMCOUNT, useNA = "always")
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FAMCOUNT") 
  unique(tmp_16$famcount)
  
  
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_20 = list(tmp_1920,tmp_1718,tmp_16) %>% bind_rows()
  with(tmp_16_20, table(year,famcount, useNA = "always"))
  
  
  with(tmp_16_20, unique(famcount))
  tmp_16_20 = tmp_16_20 %>% 
    dplyr::mutate(famcount = famcount %>% zap_attributes() %>% 
                    haven::labelled(labels = c("8 or more"=8), 
                                    label = "Number of family members in household"))
  with(tmp_16_20, unique(famcount))
  
  nrow(cahmi_2016_2020)==nrow(tmp_16_20)
  nrow(cahmi_2016_2020)
  cahmi_2016_2020 = cahmi_2016_2020 %>% 
    left_join(tmp_16_20, by = c("year","fipsst","stratum","hhid", "fwc")) 
  nrow(cahmi_2016_2020)
  rm(list = ls()[startsWith(ls(),"tmp")])
  
  
  if(save_out){
  ##### Save out thie data set ####
  readr::write_rds(cahmi_2016_2020, file = paste0(path,"/data-files/intermediate-files/cahmi_2016_2020.RDS"))
  }
}