
get_reason_10a <- function(df_in, show_plot = F, return_scores = F){

    # Reason 10a is triggered if a score from a rasch model to developmental items (with trajectories) results in a score outside 
    #            of the range observed from UNMC2020 study.
    
    df = df_in
    
    item_crosswalk = readr::read_csv(file = "C:/Users/marcu/Dropbox/UNMC/phase-2/data-files/intermediate-files/items_lex_crosswalk.csv")
    list_fitted_rasch = readRDS(file = "C:/Users/marcu/Dropbox/UNMC/phase-2/data-files/intermediate-files/list_fitted_extended_rasch_unmc20.RDS")
    
    
    items_20 = list_fitted_rasch$dat %>% dplyr::select(-(ResponseId:log_yrs)) %>% names()
    
    lex_map = data.frame(lex_from = item_crosswalk$lex_ne22[!is.na(item_crosswalk$lex_ne20)], 
                         lex_to = item_crosswalk$lex_ne20[!is.na(item_crosswalk$lex_ne20)])
    
    names(df) = plyr::mapvalues(names(df), from = lex_map$lex_from, to = lex_map$lex_to)
    
    df = df %>% 
      dplyr::filter(days>1 & days<(2190+14)) %>% 
      dplyr::mutate(source = "UNMC22", yrs = days/365.25, log_yrs = log(yrs+1)) %>% 
      dplyr::select(source, ResponseId, yrs, log_yrs, all_of(items_20)) %>% 
      bind_rows(list_fitted_rasch$dat %>% dplyr::mutate(source = "UNMC2020")) %>% 
      dplyr::mutate(source = factor(source))
    
    # Get rid of observations with response patterns that have only missing
    nresp = apply(df %>% dplyr::select(C1:NOM40), 1, function(x){sum(!is.na(x))})
    df = df[nresp>0, ]
    
    
    fit_extend2 = mirt::mirt(data = df %>% dplyr::select(-(source:log_yrs)),
                       model = mirt::mirt.model(input = "f = 1-180"),
                       pars = list_fitted_rasch$mirt_estimates,
                       itemtype = "Rasch",
                       covdata = df %>% dplyr::select(yrs,log_yrs),
                       formula = ~yrs + log_yrs)
    
    df = df %>% 
      dplyr::mutate(sanity_scores = mirt::fscores(fit_extend2, method = "EAP") %>% as.vector())

    obj_loess = loess(sanity_scores~yrs, data = df %>% dplyr::filter(source == "UNMC2020"))
    
    df = df %>% dplyr::mutate(sanity_hat = predict(obj_loess, newdata = df))
    
    df = df %>% dplyr::mutate(sanity_eps = sanity_scores-sanity_hat)
    
    max_eps = max((df$sanity_eps[df$source=="UNMC2020"]))
    min_eps = min((df$sanity_eps[df$source=="UNMC2020"]))
    
    df = df %>% dplyr::filter(source == "UNMC22") %>% 
      dplyr::mutate(reason_10a = ifelse(sanity_eps<min_eps | sanity_eps>max_eps, 1,0))
      
    if(show_plot){
      ggplot(df %>% dplyr::filter(source=="UNMC22"), aes(x = yrs, y = sanity_eps)) + 
        geom_point(alpha = 0.3) + stat_smooth(method = "loess", se = F) +
        geom_hline(yintercept = max_eps, shape = 2, col = "red") +
        geom_hline(yintercept = min_eps, shape = 2, col = "red") + 
        labs(title = "Sanity Score Outlier Analysis")
    }
    
    if(return_scores){
      return(df %>% dplyr::select(ResponseId, reason_10a, sanity_scores, sanity_eps))
      
    } else {
      return(df %>% dplyr::select(ResponseId, reason_10a))
    }

}