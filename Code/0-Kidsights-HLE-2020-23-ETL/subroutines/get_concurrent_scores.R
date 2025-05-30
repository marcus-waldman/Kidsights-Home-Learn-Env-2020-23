get_concurrent_scores<-function(survey_out_df, intermediate_path, idvariable = "ResponseId"){
  
  
  # Add psychosocial scores to survey responses
  survey_out_df = survey_out_df %>%
    dplyr::left_join(score_psy(df_in = survey_out_df, idvar = idvariable), by = idvariable)
  
  # Add in D-scores
  gsed_df = survey_out_df %>% dplyr::filter(days < (42/12)*365.25 )
  out_dscore = get_dscores(input_df = gsed_df)
  survey_out_df = survey_out_df %>%
    dplyr::left_join(out_dscore, by = idvariable)
  
  # Add in CREDI Scores
  out_credi = get_credi_scores(input_df = survey_out_df)
  survey_out_df = survey_out_df %>%
    dplyr::left_join(out_credi, by = idvariable)
  
  # Add in NOM
  out_NOM = nom_fscores(df=survey_out_df %>% dplyr::filter(12*(days/365.25)>=36 & 12*(days/365.25)<72 & eligible==1), idvar=idvariable)
  survey_out_df = survey_out_df %>%
    dplyr::left_join(out_NOM, by = idvariable)
  
  # Add in ECDI score
  out_ECDI = ecdi_fscores(df=survey_out_df %>% dplyr::filter(eligible==1 & 12*(days/365.25) >= 25 & 12*(days/365.25)<60), idvar=idvariable, intermediate_path = "C:/Users/marcu/Dropbox/UNMC/phase-2/data-files/intermediate-files")
  survey_out_df = survey_out_df %>%
    dplyr::left_join(out_ECDI, by = idvariable)
  
  # Get residuals
  out_eps = get_eps(df = survey_out_df, idvar = "ResponseId", which_scores = c("dscore","credi_OVERALL", "nomlearn","nomphys","nomsem","nomsreg", "ecdi"), eligible_only = T)
  survey_out_df = survey_out_df %>%
    dplyr::left_join(out_eps, by = "ResponseId")
  
  return(survey_out_df)
  
}

