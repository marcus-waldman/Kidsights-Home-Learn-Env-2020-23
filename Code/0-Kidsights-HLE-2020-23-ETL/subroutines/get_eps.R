
# df = elig_list$survey
# idvar = "ResponseId"
# which_scores = c("kidsight_fscore","dscore","credi_OVERALL")
get_eps<-function(df,idvar = "ResponseId",which_scores = c("kidsight_fscore","dscore","credi_OVERALL"), eligible_only = T){
  library(tidyverse)  
  library(gamlss)

  if(eligible_only){
    df = df %>% dplyr::filter(eligible==1)
  }
  df = df %>% dplyr::mutate(yrs = days/365.25)
  out = df %>% dplyr::select(ResponseId)
  
  print("Fitting GAMLSS models for age-adjusted scores.")

  for(var_j in which_scores){
    df_j = df %>% dplyr::select(all_of(idvar), yrs,all_of(var_j)) %>% na.omit()  
    names(df_j) = c("id","x","y")
    fit_j = gamlss(y~pbm(x, mono = "up"), sigma.formula = y~pbm(x), data = df_j) 
    if(nrow(df_j) != length(fit_j$residuals)){stop("nrow(df_j) != length(fit_j$residuals)")}
    eps_j = data.frame(id = df_j$id, eps = fit_j$residuals)
    names(eps_j) = c(idvar,paste0(var_j, "_eps"))
    out = out %>% dplyr::left_join(eps_j, by = idvar)
    
  }
  
  
  return(out)
}