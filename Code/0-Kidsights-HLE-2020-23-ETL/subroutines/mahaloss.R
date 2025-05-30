# Assess loss
mahaloss<-function(theta,impdat,e_df, vars, mu, Sigma){
  
  
  require(tidyverse)
  
  phi = exp(theta)
  phi = phi/(1+sum(phi))
  phi = c(1-sum(phi),phi)
  
  es = as.matrix(e_df %>% dplyr::select(starts_with("e"))) %*% phi
  # print(phi)
  # print(head(sort(es,decreasing = T)))
  ewgt_df = data.frame(uid = e_df$uid, ewgt = as.numeric(es/(1-es)))
    
  impdat = impdat %>% 
    dplyr::left_join(ewgt_df, by = "uid")
  
  impdat = impdat %>% 
    dplyr::mutate(wgt = ifelse(acs_nsch, wgt, ewgt)) %>% 
    dplyr::select(-ewgt)
  

  loss_out <- sapply(1:M, function(m){
    df_m = impdat %>% dplyr::filter(acs_nsch==F, .imp==m)%>% 
      dplyr::select(wgt, sc_age_years, all_of(vars))
    fit_m = lm(cbind(govt,loginc,rural,chhealth,n_aces,a1_menthealth,a1_physhealth,a1_parent,hs_or_less,somecoll,bs,white,black,hisp,care10hrs)~-1+as.factor(sc_age_years), 
               weights = df_m$wgt, 
               data = df_m)
    loss_m = 0
    for(a in 1:6){
      loss_m = loss_m + stats::mahalanobis(fit_m$coefficients[a,], center = mu[a,], cov = Sigma)
    }
    return(loss_m)
  }) %>% mean()
  
  return(loss_out)
  
}