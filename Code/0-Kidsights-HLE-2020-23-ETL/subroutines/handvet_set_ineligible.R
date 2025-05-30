handvet_set_ineligible<-function(survey_df, ids){
  survey_df = survey_df %>% 
    dplyr::mutate(eligible = ifelse(ResponseId %in% ids, F, eligible))
}