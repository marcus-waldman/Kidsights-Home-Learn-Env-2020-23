handvet_set_eligible<-function(survey_df, ids){
  survey_df = survey_df %>% 
    dplyr::mutate(eligible = ifelse(ResponseId %in% ids, T, eligible))
}