evaluate_eligibility<-function(screener_df, survey_df, ref_df, IP_Hub, zipcodeR_tigris, zipcodes_df, score_it = T){
  
  require(tidyverse)
  require(lubridate)
  require(pbapply)
  
  ###############################################################################
  # Partition screener data and initialize a list documenting ineligible reasons
  
  #Construct three datasets: From screener_df: 
  #   (1) "Just screener" data (just_screener_df) - 
  #       Only observations of have filled out SQ questions from the SCREENER pull
  #   (2) Survey responses in SCREENER pull (survey_n_screener_df)
  #       Observations in the SCREENER pull that have answered screener questions
  #   (3) Pulls from the SURVEY (survey_df)
  #       Observations who responsed to an invited email with link to the survey.
  
  dtm_screen = lubridate::make_datetime(year = 2022, month=8, day=26, hour=10, min=0)
  just_screener_df = screener_df %>% 
    dplyr::filter(StartDate>=dtm_screen & Progress<85)
  
  survey_n_screener_df = screener_df %>% 
    dplyr::filter( !(ResponseId %in% just_screener_df$ResponseId) ) %>% 
    recode_qualtrics_survey(reference_df = ref_df, zipcodeR_tigris = zipcodeR_tigris) %>% 
    recode_items()
  
  # Complete some sanity checks.
  if(!identical(nrow(screener_df), nrow(just_screener_df)+nrow(survey_n_screener_df))){
    error("Improperly partitioned screener_df")
  } else{
    #Note that screening was implemented 26 Aug 2022 at 10am. 
    # So the minimum date.time for the just_screener_df shuold be after that value.
    if(min(just_screener_df$StartDate)<dtm_screen){
      error("just_screener_df has a date.time before screener was implemented.")
    } 
  }
  
  #Initialize a list for documenting ineligble reasons according to codes by KH's protocol
  ineligible_reason = list(
    just_screener = data.frame(ResponseId = just_screener_df$ResponseId) %>% 
      dplyr::mutate(reason_0a = 0, reason_0b = 0, reason_1a = 0, reason_1b = 0, reason_2a = 0, reason_2b = 0, reason_3a = 0, reason_4a = 0, reason_5a = 0, reason_6a=0, reason_7a=0, reason_8a=0, reason_9a=0, reason_10a=0), 
    survey_n_screener = data.frame(ResponseId = survey_n_screener_df$ResponseId) %>% 
      dplyr::mutate(reason_0a = 0, reason_0b = 0, reason_1a = 0, reason_1b = 0, reason_2a = 0, reason_2b = 0, reason_3a = 0, reason_4a = 0, reason_5a = 0, reason_6a=0, reason_7a=0, reason_8a=0, reason_9a=0, reason_10a=0),
    survey = data.frame(ResponseId = survey_df$ResponseId) %>% 
      dplyr::mutate(reason_0a = 0, reason_0b = 0, reason_1a = 0, reason_1b = 0, reason_2a = 0, reason_2b = 0, reason_3a = 0, reason_4a = 0, reason_5a = 0, reason_6a=0, reason_7a=0, reason_8a=0, reason_9a=0, reason_10a=0)
  )
  
  
  #################################################################################
  # 0.a - Does not consent to survey 
  # 0.b - Not a primary caregiver
  
  # survey
  ineligible_reason$survey$reason_0a[survey_df$EQ001==2] = 1
  ineligible_reason$survey$reason_0b[survey_df$EQ002==2] = 1
  #ineligible_reason$survey$reason_0a[is.na(survey_df$EQ001)] = 1
  #ineligible_reason$survey$reason_0b[is.na(survey_df$EQ002)] = 1
  
  # survey_n_screener !! Where are these variables !!!
  #ineligible_reason$survey_n_screener$reason_0a[survey_n_screener_df$EQ001==2] = 1
  ineligible_reason$survey_n_screener$reason_0b[survey_n_screener_df$EQ002==2] = 1
  #ineligible_reason$survey_n_screener$reason_0a[is.na(survey_n_screener_df$EQ001)] = 1
  #ineligible_reason$survey_n_screener$reason_0b[is.na(survey_n_screener_df$EQ002)] = 1
  
  
  
  ################################################################################
  # 1. Duplicate IP Address 
  # From just screener pull PULL Variable IPAddress – mark any duplicates as ineligible
  
  # Reason 1_a: Multiple IP address in corresponding data set
  IP_freq_just_screener = just_screener_df %>% 
    dplyr::group_by(IPAddress) %>% 
    dplyr::summarise(IP_Freq = length(IPAddress)) %>% 
    dplyr::filter(IPAddress != "") %>% 
    arrange(-IP_Freq) 
  ineligible_reason$just_screener$reason_1a[just_screener_df$IPAddress %in% IP_freq_just_screener$IPAddress[IP_freq_just_screener$IP_Freq>1]] = 1
  
  IP_freq_survey_n_screener = survey_n_screener_df %>% 
    dplyr::group_by(IPAddress) %>% 
    dplyr::summarise(IP_Freq = length(IPAddress)) %>% 
    dplyr::filter(IPAddress != "") %>% 
    arrange(-IP_Freq) 
  ineligible_reason$survey_n_screener$reason_1a[survey_n_screener_df$IPAddress %in% IP_freq_survey_n_screener$IPAddress[IP_freq_survey_n_screener$IP_Freq>1]] = 1
  
  IP_survey = survey_df %>% 
    dplyr::group_by(IPAddress) %>% 
    dplyr::summarise(IP_Freq = length(IPAddress)) %>% 
    dplyr::filter(IPAddress != "") %>% 
    arrange(-IP_Freq) 
  ineligible_reason$survey$reason_1a[survey_df$IPAddress %in% IP_survey$IPAddress[IP_survey$IP_Freq>1]] = 1
  
  #Reason 1_b
  IPs_appears_in_surveynscreen_or_survey_and_justscreen = 
    intersect(IP_freq_just_screener$IPAddress, unique(IP_freq_survey_n_screener$IPAddress, IP_survey$IPAddress))
  ineligible_reason$just_screener$reason_1b[just_screener_df$IPAddress %in% IPs_appears_in_surveynscreen_or_survey_and_justscreen] = 1
  
  IPs_appear_in_survey_and_surveynscreen = 
    intersect(IP_survey$IPAddress, IP_freq_survey_n_screener$IPAddress)
  ineligible_reason$survey$reason_1b[survey_df$IPAddress %in% IPs_appear_in_survey_and_surveynscreen] = 1
  
  #Reason 1_b
  IPs_appears_in_surveynscreen_and_justscreen = 
    intersect(IP_freq_just_screener$IPAddress, IP_freq_survey_n_screener$IPAddress)
  ineligible_reason$just_screener$reason_1b[just_screener_df$IPAddress %in% IPs_appears_in_surveynscreen_and_justscreen] = 1
  
  IPs_appear_in_survey_and_surveynscreen = 
    intersect(IP_survey$IPAddress, IP_freq_survey_n_screener$IPAddress)
  ineligible_reason$survey$reason_1b[survey_df$IPAddress %in% IPs_appear_in_survey_and_surveynscreen] = 1
  
  
  
  ################################################################################
  # 2. Incomplete responses or completed survey too quickly
  # -a. For SCREENER Survey 
  #   -- i. Within SURVEY if they filled out the survey questionnaire, or within SURVEY, anything below 85 on the progress variable, Progress, mark as ineligible 
  ineligible_reason$survey_n_screener$reason_2a[survey_n_screener_df$Progress<85] = 1
  ineligible_reason$survey$reason_2a[survey_df$Progress<85] = 1
  
  
  #   -- ii. Within SCREENER, if only filled out screener questions then anything below 100 on the variable, Progress, mark as ineligible 
  ineligible_reason$just_screener$reason_2a[just_screener_df$Progress<1] = 1
  #!!Note that this may not make a lot of sense because some people get passed on to the full survey now.
  
  # -b.For survey_n_screener and survey, this should take at least 264 seconds (4min and 24 seconds based on 2019-2020 administration in Lincoln and Omaha)
  ineligible_reason$survey_n_screener$reason_2b[survey_n_screener_df$Duration__in_seconds_<264] = 1
  ineligible_reason$survey$reason_2b[survey_df$Duration__in_seconds_<264] = 1
  
  
  ################################################################################
  #
  # 3. Screen out children too young or old. 
  #     Mark any observations where NO child is more than 1 day and less than (2190 + 14)
  #     days as ineligible 
  #
  
  # Screened individual must have at least one child in the eligible range
  just_screener_eligible_Ids = 
    lengthen_recode_qualtrics_screener(just_screener_df, reference_df = ref_df, zipcodeR_tigris = zipcodeR_tigris) %>% 
    dplyr::group_by(ResponseId) %>% 
    dplyr::summarise(num_child_elig = sum(days>1 & days<(2190+14))) %>% 
    dplyr::filter(num_child_elig>=1) %>% 
    purrr::pluck("ResponseId")
  ineligible_reason$just_screener$reason_3a[ !(just_screener_df$ResponseId %in% just_screener_eligible_Ids) ] = 1
  
  # Evaluate children's ages in survey_n_screener
  ineligible_reason$survey_n_screener$reason_3a[ !(survey_n_screener_df$days>1 & survey_n_screener_df$days<(2190 + 14)) ] = 1
  
  
  # Evaluate children's ages in survey
  ineligible_reason$survey$reason_3a[ !(survey_df$days>1 & survey_df$days<(2190 + 14)) ] = 1
  
  ################################################################################
  #
  # 4. a. Screen out those without a Nebraska zip code (either in KH database or in zipcodeR)
  #
  # 
  combined_zipcodes = c(zipcodes_df$ZipCode %>% as.character(), zipcodeR::search_state("NE") %>% purrr::pluck("zipcode")) %>% unique()  
  ineligible_reason$just_screener$reason_4a[!with(just_screener_df, as.character(SQZIP) %in% combined_zipcodes)] = 1
  ineligible_reason$survey_n_screener$reason_4a[!with(survey_n_screener_df, as.character(SQ001) %in% combined_zipcodes)] = 1
  ineligible_reason$survey$reason_4a[!with(survey_df, as.character(SQ001) %in% combined_zipcodes)] = 1
  
  
  ################################################################################
  #
  # 5. a.  For observations that filler out the survey either in SURVEY or SCREENER, 
  #        mark any observations that report zip code in incorrect or nonadjacent county. 
  #        Also mark inelgibile any observation where the county is not reported.
  
  
  # Screener
  # First, set ineligible reason to 999 if screened out because of non-Nebraska zip code
  ineligible_reason$survey_n_screener$reason_5a[ineligible_reason$survey_n_screener$reason_4a==1] = 999
  ineligible_reason$survey$reason_5a[ineligible_reason$survey$reason_4a==1] = 999
  ineligible_reason$just_screener$reason_5a[ineligible_reason$just_screener$reason_4a==1] = 999
  
  #Second, conduct some pre processing
  KH_zipcodes= unique(zipcodes_df$ZipCode) %>% sort()
  acceptable_counties_list = pblapply(KH_zipcodes, function(zip){
    zipcodes_df %>% dplyr::filter(ZipCode==zip) %>% purrr::pluck("County") %>% return()
  })
  names(acceptable_counties_list) = KH_zipcodes
  
  # Third, now for the survey_n_screener, look to see if either county provided by FQ001 or SQCOUNT is an acceptable count
  FQ_counties_stated_survey_n_screener = with(survey_n_screener_df, 
                                              plyr::mapvalues(FQ001 %>% as.integer(), 
                                                              from = FQ001 %>%  sjlabelled::get_labels(values = "n") %>% names() %>% as.integer(), 
                                                              to = FQ001 %>% sjlabelled::get_labels(),
                                                              warn_missing = F) 
  ) %>% 
    stringr::str_remove_all(" County") %>% 
    stringr::str_remove_all(", including the city of Lincoln")
  SQ_counties_stated_survey_n_screener = with(survey_n_screener_df, 
                                              plyr::mapvalues(SQCOUNT %>% as.integer(), 
                                                              from = SQCOUNT %>%  sjlabelled::get_labels(values = "n") %>% names() %>% as.integer(), 
                                                              to = SQCOUNT %>% sjlabelled::get_labels(),
                                                              warn_missing = F) 
  ) %>% 
    stringr::str_remove_all(" County") %>% 
    stringr::str_remove_all(", including the city of Lincoln")
  idx = which(ineligible_reason$survey_n_screener$reason_4a==0)
  for(i in idx){
    counties_stated_i = c(SQ_counties_stated_survey_n_screener[i], FQ_counties_stated_survey_n_screener[i])
    zip_stated_i = with(survey_n_screener_df, ifelse(SQ001[i]!="", SQ001[i], SQZIP[i]))
    acceptable_counties_i = acceptable_counties_list[[zip_stated_i]]
    if(sum(counties_stated_i %in% acceptable_counties_i, na.rm = T)==0){
      ineligible_reason$survey_n_screener$reason_5a[i] = 1
    }
  }
  
  #Fourth, For survey,evaluate whether for each Nebraska county given, it comports with one of the acceptable counties KH identified
  counties_stated_survey = with(survey_df, 
                                plyr::mapvalues(FQ001 %>% as.integer(), 
                                                from = FQ001 %>%  sjlabelled::get_labels(values = "n") %>% names() %>% as.integer(), 
                                                to = FQ001 %>% sjlabelled::get_labels(),
                                                warn_missing = F) 
  ) %>% 
    stringr::str_remove_all(" County") %>% 
    stringr::str_remove_all(", including the city of Lincoln")
  idx = which(ineligible_reason$survey$reason_4a==0)
  for(i in idx){
    county_stated_i = counties_stated_survey[i]
    zip_stated_i = survey_df$SQ001[i]
    acceptable_counties_i = acceptable_counties_list[[zip_stated_i]]
    if(!(county_stated_i %in% acceptable_counties_i)){
      ineligible_reason$survey$reason_5a[i] = 1
    }
  }
  
  #Fifth, for just_screener, evaluate if stated county comports with acceptable county
  counties_stated_just_screener = with(just_screener_df, 
                                       plyr::mapvalues( SQCOUNT %>% as.integer(), 
                                                        from = SQCOUNT %>%  sjlabelled::get_labels(values = "n") %>% names() %>% as.integer(), 
                                                        to = SQCOUNT %>% sjlabelled::get_labels(),
                                                        warn_missing = F) 
  ) %>% 
    stringr::str_remove_all(" County") %>% 
    stringr::str_remove_all(", including the city of Lincoln")
  idx = which(ineligible_reason$just_screener$reason_4a==0)
  for(i in idx){
    county_stated_i = counties_stated_just_screener[i]
    zip_stated_i = with(just_screener_df, ifelse(SQ001[i]!="", SQ001[i], SQZIP[i]))
    acceptable_counties_i = acceptable_counties_list[[zip_stated_i]]
    if(!(county_stated_i %in% acceptable_counties_i)){
      ineligible_reason$just_screener$reason_5a[i] = 1
    }
  }
  
  ###############################################################################
  # 
  # 6.a rIP package authors recommends blocking based on IP_Hub information
  # 7.a ip2Location is outside of Nebraska
  # 
  
  # just_screener
  tmp = data.frame(ResponseId = just_screener_df$ResponseId, IPAddress = just_screener_df$IPAddress) %>% 
    dplyr::left_join(IP_Hub, by = "IPAddress") 
  ineligible_reason$just_screener$reason_6a[tmp$IP_Hub_recommend_block!=0] = 1
  ineligible_reason$just_screener$reason_7a[tmp$ip2_region != "Nebraska"] = 1
  
  # survey_n_screener
  tmp = data.frame(ResponseId = survey_n_screener_df$ResponseId, IPAddress = survey_n_screener_df$IPAddress) %>% 
    dplyr::left_join(IP_Hub, by = "IPAddress") 
  ineligible_reason$survey_n_screener$reason_6a[tmp$IP_Hub_recommend_block!=0] = 1
  ineligible_reason$survey_n_screener$reason_7a[tmp$ip2_region != "Nebraska"] = 1
  
  # survey
  tmp = data.frame(ResponseId = survey_df$ResponseId, IPAddress = survey_df$IPAddress) %>% 
    dplyr::left_join(IP_Hub, by = "IPAddress") 
  ineligible_reason$survey$reason_6a[tmp$IP_Hub_recommend_block!=0] = 1
  ineligible_reason$survey$reason_7a[tmp$ip2_region != "Nebraska"] = 1
  
  
  ###############################################################################
  # 
  # 8.a Zip code at end of survey for mailing gift code is not a Nebraska zip code
  # 
  
  ineligible_reason$survey$reason_8a = as.integer( !(survey_df$Q1398 %in% unique(zipcodes_df$ZipCode)))
  ineligible_reason$survey_n_screener$reason_8a = as.integer( !(survey_n_screener_df$Q1398 %in% unique(zipcodes_df$ZipCode)))
  ineligible_reason$just_screener$reason_8a = as.integer( !(just_screener_df$Q1398 %in% unique(zipcodes_df$ZipCode)))
  
  
  ###############################################################################
  # a
  # 9.a For survey and survey_n_screener, identify observations with multiple and inconsistent responses items across age forms
  # 
  
  # Survey
  reason9a_Ids= find_multi_inconsis_ageform_responses(survey_df) %>% 
    dplyr::filter(multi_inconsis==1) %>% 
    purrr::pluck("ResponseId") %>% 
    as.character()
  ineligible_reason$survey$reason_9a[ineligible_reason$survey$ResponseId %in% reason9a_Ids] = 1
  
  
  # survey_n_screener
  reason9a_Ids= find_multi_inconsis_ageform_responses(survey_n_screener_df) %>% 
    dplyr::filter(multi_inconsis==1) %>% 
    purrr::pluck("ResponseId") %>% 
    as.character()
  ineligible_reason$survey_n_screener_df$reason_9a[ineligible_reason$survey_n_screener_df$ResponseId %in% reason9a_Ids] = 1
  
  ###############################################################################
  # 
  # 10.a Responses to items reflect scores which warrant further scrutiny before being allowed to be included
  # 
  
  #survey_df
  reason10a_Ids = get_reason_10a(survey_df) %>% 
    dplyr::filter(reason_10a==1) %>% 
    purrr::pluck("ResponseId")
  ineligible_reason$survey$reason_10a[ineligible_reason$survey$ResponseId %in% reason10a_Ids] = 1
  #Later need to get a list of acceptable IDs and set those back to zero after vetting
  
  #survey_n_screener
  reason10a_Ids = get_reason_10a(survey_n_screener_df) %>% 
    dplyr::filter(reason_10a==1) %>% 
    purrr::pluck("ResponseId")
  ineligible_reason$survey_n_screener$reason_10a[ineligible_reason$survey_n_screener$ResponseId %in% reason10a_Ids] = 1
  #Later need to get a list of acceptable IDs and set those back to zero after vetting
  
  
  ################################################################################
  # Determine eligiblity for each
  eligiblity_df = ineligible_reason$survey %>% dplyr::mutate(source = "SURVEY") %>% 
    bind_rows(ineligible_reason$survey_n_screener %>% dplyr::mutate(source = "SCREENER with survey responses")) %>% 
    bind_rows(ineligible_reason$just_screener %>% dplyr::mutate(source = "SCREENER but no survey responses"))
  eligiblity_df = eligiblity_df %>% dplyr::mutate(eligible = 0, pathway = "ineligible")
  
  #Path I: reason0_a:reason5_a + reason_9a all satisfied 
  eligiblity_df$eligible[with(eligiblity_df, reason_0a==0 & reason_0b==0 &reason_1a==0 & reason_1b==0 & reason_2a==0 & reason_2b==0 & reason_3a==0 & reason_4a==0 & reason_5a==0 & reason_9a==0)]=1
  eligiblity_df$pathway[with(eligiblity_df, reason_0a==0 & reason_0b==0 & reason_1a==0 & reason_1b==0 & reason_2a==0 & reason_2b==0 & reason_3a==0 & reason_4a==0 & reason_5a==0 & reason_9a==0)]="I"
  
  
  #Path II: reason0_a to reason4_a + reason_9a all satisifed, 
  #         but reason5_a not (doesn't know county); however, 
  #         reason6_a (not recommended blocked) and reason7_a (NE IP address) satisitfied 
  eligiblity_df$eligible[with(eligiblity_df, reason_0a==0 & reason_0b==0 & reason_1a==0 & reason_1b==0 & reason_2a==0 & reason_2b==0 & reason_3a==0 & reason_4a==0 & reason_5a==1 & reason_6a==0 & reason_7a==0 & reason_9a==0)]=1
  eligiblity_df$pathway[with(eligiblity_df, reason_0a==0 & reason_0b==0 & reason_1a==0 & reason_1b==0 & reason_2a==0 & reason_2b==0 & reason_3a==0 & reason_4a==0 & reason_5a==1 & reason_6a==0 & reason_7a==0 & reason_9a==0)]="II"
  
  
  #Path III: reason0_a to reason4_a all satisfied, but reason5_a not (doesn't know county), but reason8_a is satisitfied 
  eligiblity_df$eligible[with(eligiblity_df, reason_0a==0 & reason_0b==0 & reason_1a==0 & reason_1b==0 & reason_2a==0 & reason_2b==0 & reason_3a==0 & reason_4a==0 & reason_5a==1 & reason_8a==0 & reason_9a==0)]=1
  eligiblity_df$pathway[with(eligiblity_df, reason_0a==0 & reason_0b==0 & reason_1a==0 & reason_1b==0 & reason_2a==0 & reason_2b==0 & reason_3a==0 & reason_4a==0 & reason_5a==1 & reason_8a==0 & reason_9a==0)]="III"
  
  # Regardless of path, if outlier (reason_10a = 1), set set eligiblity to 0, and upate the path to include an outlier
  eligiblity_df$pathway[eligiblity_df$eligible==1 & eligiblity_df$reason_10a==1] = "ineligible until vetted"
  eligiblity_df$eligible[eligiblity_df$eligible==1 & eligiblity_df$reason_10a==1] = 0
  
  
  

  ##############################################################################
  # Provided that a duplicate IP Address meets all aother eligibility criteria, set the first recorded
  # record that meets all eligiblity criteria to eligible
  
   survey_out_df =  bind_rows(survey_df, survey_n_screener_df) %>% 
    left_join(eligiblity_df, by = "ResponseId")
    
    # Survey
      count_IP = survey_out_df %>% 
      dplyr::group_by(IPAddress) %>% 
      dplyr::summarize(n_IP = length(IPAddress)) %>% 
      dplyr::filter(IPAddress != "") %>% 
      arrange(-n_IP)
    
      multiple_IPs = survey_out_df %>% 
          dplyr::left_join(count_IP, by = "IPAddress") %>% 
          dplyr::filter(n_IP>1) %>% 
          dplyr::mutate(  pathwayI =   (reason_0a==0 & reason_0b==0 & reason_2a==0 & reason_2b==0 & reason_3a==0 & reason_4a==0 & reason_5a==0 & reason_9a==0 & reason_10a==0), 
                          pathwayII =  (reason_0a==0 & reason_0b==0 & reason_2a==0 & reason_2b==0 & reason_3a==0 & reason_4a==0 & reason_5a==1 & reason_6a==0 & reason_7a==0 & reason_9a==0 & reason_10a==0), 
                          pathwayIII = (reason_0a==0 & reason_0b==0 & reason_2a==0 & reason_2b==0 & reason_3a==0 & reason_4a==0 & reason_5a==1 & reason_8a==0 & reason_9a==0 & reason_10a==0), 
                          set_eligible = pathwayI | pathwayII | pathwayIII
          ) %>% 
          dplyr::filter(set_eligible) %>% 
          dplyr::mutate(pathway = ifelse(pathwayI, "I", ifelse(pathwayII,"II", "III"))) %>% 
          dplyr::group_by(IPAddress) %>% 
          dplyr::summarize(ResponseId = ResponseId[which.min(StartDate)], 
                           set_pathway = pathway[which.min(StartDate)]) %>% 
          dplyr::mutate(set_eligible = 1) %>% 
          dplyr::select(-IPAddress)
    
    
    survey_out_df = survey_out_df %>% 
      dplyr::left_join(multiple_IPs, by = "ResponseId")
  
    
    survey_out_df$eligible[survey_out_df$set_eligible==1]  = 1
    survey_out_df$pathway[survey_out_df$set_pathway=="I"] = "I"
    survey_out_df$pathway[survey_out_df$set_pathway=="II"] = "II"
    survey_out_df$pathway[survey_out_df$set_pathway=="III"] = "III"
  
    
 ##############################################################################
 # Get scores from various measures
  if (score_it){
      # Add psychosocial scores to survey responses
      survey_out_df = survey_out_df %>%
        dplyr::left_join(score_psy(df_in = survey_out_df, idvar = "ResponseId"), by = "ResponseId")
    
      # Add in D-scores
      gsed_df = survey_out_df %>% dplyr::filter(days < (42/12)*365.25 )
      out_dscore = get_dscores(input_df = gsed_df)
      survey_out_df = survey_out_df %>%
        dplyr::left_join(out_dscore, by = "ResponseId")
    
      # Add in CREDI Scores
      out_credi = get_credi_scores(input_df = survey_out_df)
      survey_out_df = survey_out_df %>%
        dplyr::left_join(out_credi, by = "ResponseId")
    
      # # Add in Kidsight factor scores
      # out_kidsight = kidsight_fscores(df=survey_out_df,idvar="ResponseId",intermediate_path = "C:/Users/marcu/Dropbox/UNMC/phase-2/data-files/intermediate-files")
      # survey_out_df = survey_out_df %>%
      #   dplyr::left_join(out_kidsight, by = "ResponseId")
    
      # Add in NOM
      out_NOM = nom_fscores(df=survey_out_df %>% dplyr::filter(12*(days/365.25)>=36 & 12*(days/365.25)<72 & eligible==1), idvar="ResponseId")
      survey_out_df = survey_out_df %>%
        dplyr::left_join(out_NOM, by = "ResponseId")
    
      # Add in ECDI score
      out_ECDI = ecdi_fscores(df=survey_out_df %>% dplyr::filter(eligible==1 & 12*(days/365.25) >= 25 & 12*(days/365.25)<60), idvar="ResponseId", intermediate_path = "C:/Users/marcu/Dropbox/UNMC/phase-2/data-files/intermediate-files")
      survey_out_df = survey_out_df %>%
        dplyr::left_join(out_ECDI, by = "ResponseId")
    
      # Get residuals
      out_eps = get_eps(df = survey_out_df, idvar = "ResponseId", 
                        which_scores = c(#"kidsight_fscore",
                                        "dscore","credi_OVERALL", "nomlearn","nomphys","nomsem","nomsreg", "ecdi"
                                        ), eligible_only = T)
      survey_out_df = survey_out_df %>%
        dplyr::left_join(out_eps, by = "ResponseId")
  } # End score_it
    
    
  # Return a list
  out_list = list(survey = survey_out_df,
                  screener_long = just_screener_df %>%
                    lengthen_recode_qualtrics_screener(reference_df = ref_df, zipcodeR_tigris = zipcodeR_tigris) %>%
                    left_join(eligiblity_df, by = "ResponseId"),
                  elibility_dataframe = eligiblity_df
              )

  return(out_list)
  
  
}