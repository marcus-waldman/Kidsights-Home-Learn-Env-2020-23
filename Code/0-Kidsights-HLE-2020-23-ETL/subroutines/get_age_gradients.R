get_age_gradients<-function(item_crosswalk, path = NULL, filename = "age_gradients.pdf"){
  
    item_crosswalk = item_crosswalk %>% dplyr::mutate(resp_opts = "", age_coef = NA)
    if(!is.null(path)){
      pdf(file = paste0(path,"/",filename,".pdf"), height = 8, width = 8)
    }
    
    J = nrow(item_crosswalk)
    for(j in 1:J){
      var_j = item_crosswalk$lex_ne22[j] 
      df_j = survey_df %>% dplyr::mutate(x = days/365.25, y = survey_df[,var_j]) %>% dplyr::select(x,y) %>% stats::na.omit()
      K_j = length( unique(df_j$y) )
      n_j = nrow(df_j)
      labs_j =  paste0(sjlabelled::get_labels(df_j$y, values = "as.prefix"), collapse = "; ")
      beta_j = NA
      stars_j = NA
      if(n_j > 20){
    
        if(K_j == 2){
          df_j$y[which.min(df_j$y)] = 0; df_j$y[which.max(df_j$y)] = 1
          fit0_j = glm(formula = y~1, data = df_j, family = "binomial")
          fit1_j = glm(formula = y~x, data = df_j, family = "binomial")
          beta_j = coef(fit1_j)["x"]
          test_j = lmtest::lrtest( fit0_j, fit1_j)
          stars_j = get_stars(test_j$`Pr(>Chisq)` %>% na.omit())
          txt_j =  paste0(round(beta_j,2),stars_j)
         
          if(!is.null(path)){
            plot_j = ggplot(df_j, aes(x=x,y=y)) + 
              geom_point(alpha = 0.4) + 
              stat_smooth(se = F, method ="glm", method.args = (family = "binomial")) + 
              labs(x = "Years", y = labs_j, title = paste0(var_j, ": ", item_crosswalk$label[j]), subtitle = paste0("Est. = ", txt_j))
            print(plot_j)
          }
          
        } 
        if (K_j > 2){
          obs_ys = sort(unique(df_j$y))
          df_j$y = as.ordered(plyr::mapvalues(x = df_j$y, from = obs_ys, to = seq(0,length(obs_ys)-1)))
          fit0_j = MASS::polr(formula = y~1, data = df_j, method = "logistic")
          fit1_j = MASS::polr(formula = y~x, data = df_j, method = "logistic")
          beta_j = coef(fit1_j)["x"]
          test_j = lmtest::lrtest( fit0_j, fit1_j)
          stars_j = get_stars(test_j$`Pr(>Chisq)` %>% na.omit())
          txt_j =  paste0(round(beta_j,2),stars_j)
          if(!is.null(path)){
            plot_j = ggplot(df_j, aes(x=x,y=as.numeric(y)-1)) + 
              geom_point(alpha = 0.4) + 
              stat_smooth(se = F, method ="loess", span = 0.6, method.args = list(degree=1)) + 
              labs(x = "Years", y = labs_j, title = paste0(var_j, ": ", item_crosswalk$label[j]), subtitle = paste0("Est. = ", txt_j))
            print(plot_j)
          }
        }
        
      }
      item_crosswalk$resp_opts[j] = labs_j
      if(!is.na(beta_j)){item_crosswalk$age_coef[j] = txt_j}
    }

    
    if(!is.null(path)){
      dev.off()
    }
    
    
    return(item_crosswalk %>% dplyr::relocate(lex_ne22, age_coef, label, resp_opts))
}


