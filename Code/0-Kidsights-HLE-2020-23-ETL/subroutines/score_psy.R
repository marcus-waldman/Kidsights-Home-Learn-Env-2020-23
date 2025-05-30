score_psy <- function(df_in, idvar = "ResponseId"){
  
  library(mirt)
  library(tidyverse)
  
  df =  df_in %>% dplyr::select(all_of(idvar), starts_with("PS"))
  
  list_fitted_ps = readRDS(file = "C:/Users/marcu/Dropbox/UNMC/phase-2/data-files/intermediate-files/lav2mirt-init-psy-calib.RDS")
  PS_items = list_fitted_ps$dat %>% names()
  PS_values = list_fitted_ps$mirt %>% mirt::mod2values() %>% dplyr::mutate(est = FALSE)
  PS_model = list_fitted_ps$mirt.syntax
  
  # Get rid of observations with response patterns that have only missing
  nresp = apply(df %>% dplyr::select(dplyr::all_of(PS_items)), 1, function(x){sum(!is.na(x))})
  df = df[nresp>=5, ]
  
  fit_mirt = mirt::mirt(data = df %>% dplyr::select(all_of(PS_items)),
                        model = PS_model,
                        pars = PS_values,
                        itemtype = "graded", 
                        method = "MHRM", 
                        draws = 1)
  
  hi = mirt::fscores(fit_mirt, method = "MAP") %>% data.frame() 
  df = cbind(df,hi)
  return(df %>% dplyr::select(all_of(idvar), EAT:GEN))
}



