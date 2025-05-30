kidsight_fscores<-function(df,idvar, num.cores=8,intermediate_path = "C:/Users/marcu/Dropbox/UNMC/phase-2/data-files/intermediate-files"){
    
    require(tidyverse)
    require(doSNOW)
    require(parallel)
    # library(pbapply)
    # library(parallel)
    # library(doParallel)
  
    #doParallel::stopImplicitCluster()
  
    warning("\n !!! Warning !!! \n kidsight_fscores.R has been stripped of all parallel processing due to Framework laptop error.  \n !!!")
  
    names(df)[names(df)==idvar] = "id"
    list_params = readr::read_rds(file = paste0(intermediate_path,"/kidsights_params.rds"))
    lex_xwalk = readr::read_csv(file = paste0(intermediate_path,"/items_lex_crosswalk.csv"), show_col_types = F)
    kidsight_items =  sort(lex_xwalk$lex_kidsight)
    names(df) = plyr::mapvalues(names(df), from = lex_xwalk$lex_ne22, to = lex_xwalk$lex_kidsight )
    
    
    betas = list_params$betas
    gamma = list_params$gamma
    param_table = list_params$item
    
    df = data.frame(df) %>% 
      dplyr::select(id,days,dplyr::any_of(kidsight_items)) %>% 
      dplyr::filter((days/365.25)<6) %>% 
      dplyr::filter((days/365.25)>0) 
      
    prior_df =  df %>%
      dplyr::mutate(yrs = days/365.25, 
                    z = get_z(yrs), 
                    poly1=polyleg(z,1),
                    poly2=polyleg(z,2),
                    poly3=polyleg(z,3)) %>% 
      dplyr::mutate(mu = poly1*betas[1] + poly2*betas[2] + poly3*betas[3], 
                    sigma = 5*sqrt(exp(poly1*gamma))) %>% 
      dplyr::select(id,mu,sigma) 
    
    long_df = 
      df %>% 
      dplyr::select(id, dplyr::any_of(kidsight_items)) %>%  
      mutate(across(dplyr::any_of(kidsight_items), function(x)zap_attributes(as.integer(x)-1))) %>% 
      pivot_longer(cols = dplyr::any_of(kidsight_items), names_to = "lex_kidsight", values_to = "y") %>% 
      na.omit()
    
    if(min(long_df$y)==-1){long_df$y = long_df$y+1}
    
    long_df = long_df %>% 
      dplyr::left_join(param_table %>% dplyr::select(lex_kidsight,K:delta5), by = "lex_kidsight") %>% 
      dplyr::filter(alpha>0) %>% 
      dplyr::mutate(delta_y = ifelse(y==0, -Inf, 
                                     ifelse(y==1, delta1, 
                                            ifelse(y==2,delta2, 
                                                   ifelse(y==3, delta3, 
                                                          ifelse(y==4,delta4, 
                                                                 ifelse(y==5,delta5,NA)
                                                          )
                                                   )
                                            )
                                     )
      )
      ) %>% 
      dplyr::mutate(delta_yp1 = ifelse(y==0, delta1, 
                                       ifelse(y==1, delta2, 
                                              ifelse(y==2,delta3, 
                                                     ifelse(y==3, delta4, 
                                                            ifelse(y==4,delta5, 
                                                                   ifelse(y==5,Inf,NA)
                                                            )
                                                     )
                                              )
                                       )
      )
      ) %>% 
      dplyr::select(id,lex_kidsight,alpha, y, delta_y,delta_yp1)
    
    
    
    
    # Make sure that at least 5 items answered.
    foo = long_df %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarize(J_ans = length(y)) %>% 
      dplyr::filter(J_ans>=5)
    ids = foo$id
    
    inner_fn<-function(i, long_df, prior_df){
      library(tidyverse)
      long_i = long_df %>% dplyr::filter(id == i)
      prior_i = prior_df %>% dplyr::filter(id == i)
      mu_i = prior_i$mu
      sigma_i = prior_i$sigma
      out_optim = optim(par = mu_i, fn = neglogl, method = "BFGS", df_long = long_i, mu = mu_i, sigma = sigma_i)
      #neglogl(theta = mu_i, df_long = long_i, mu = mu_i, sigma = sigma_i)
      return(data.frame(id=i, kidsight_fscore=ifelse(out_optim$converge==0,out_optim$par,NA)))
    }
    
    #cl = parallel::makeCluster(num.cores)
    #clusterExport(cl=cl, varlist = c("long_df","prior_df","neglogl","inner_fn"), envir = environment())
    #clusterEvalQ(cl, library("tidyverse"))
    
    print("Estimating kidsight factor scores...")
#    cl = parallel::makeCluster(num.cores)
#    doSNOW::registerDoSNOW(cl)
    pb <- txtProgressBar(max=length(ids), style=3)
    progress <- function(n) setTxtProgressBar(pb, n)
    
#for(i in ids){  
#    scores_df<-
#      pblapply(ids, FUN = function(i){  
      
    scores_df <- 
      foreach::foreach(n = 1:length(ids), 
                       .packages = c("tidyverse"), 
                       .export = c("neglogl"),
                       .options.snow = list(progress=progress)) %do% { #%dopar% {
      
          
            return(inner_fn(ids[n], long_df, prior_df))
      
    } %>% dplyr::bind_rows()
   

#      }, cl = cl) 
        
#} #end for   
    #parallel::stopCluster(cl)
#    parallel::stopCluster(cl)
    names(scores_df)[1] = idvar
    
    return(scores_df)
}
