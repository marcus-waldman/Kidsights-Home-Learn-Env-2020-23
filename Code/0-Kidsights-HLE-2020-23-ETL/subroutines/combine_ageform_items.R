combine_ageform_items<-function(df){
  
  require(tidyverse)
  require(pbapply)
  require(haven)
  require(sjlabelled)
  
  
  vars = get_ageform_vars(df)

  # Bandaid for A105 corresponding to C105.1
  df$C105.1 = df$A105
  
  combined_df = pblapply(1:length(vars), function(j){
    print(j)
    
    df_j = df %>% 
      dplyr::select(starts_with(vars[j]))
    
    #Need to get the largest label
    sj_df =  lapply(1:ncol(df_j), function(jj){
      
      print(jj)
      
      data.frame(
        values = sjlabelled::get_labels(df_j[,jj], values = "n") %>% names() %>%  as.numeric(), 
        labels = sjlabelled::get_labels(df_j[,jj])
      ) %>% 
      return()
      
    }) %>% 
    dplyr::bind_rows() %>% 
    dplyr::group_by(values) %>% 
    dplyr::summarize(labels = labels[1]) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(labels) %>% 
    dplyr::summarize(values = min(values)) %>% 
    dplyr::ungroup()
    
    if(ncol(df_j)>1){
      
      nresp_vec = df_j %>%  apply(1,function(x){sum(!is.na(x))})
      
      
      if(max(nresp_vec)>1){
        nunique_vec = df_j %>% 
          apply(1, function(x){length(unique(x[!is.na(x)]))}) 
        
        if(sum(nunique_vec>1)>0){
          #Print to sent informaiton for follow up to Katelyn 
          # print(head(df %>% dplyr::filter(nunique_vec>1) %>% dplyr::select(ResponseId,StartDate, Duration__in_seconds_, days, starts_with(vars[j]))))
          
          # send out a Warning
          warning(paste0("There are ", sum(nunique_vec>1), " multple and inconsistent responses to ", vars[j]," across age forms. Setting these responses to missing."))
          
          df_j[nunique_vec>1, ] <- NA
          
        }
        nresp_vec[nresp_vec>1]=1
      }
      
      df_j_subset = df_j[nresp_vec==1, ]
      yresp_vec_subset = apply(df_j_subset,1,function(x){
        
        vals_j = unique(x) %>% na.omit()
        if(length(vals_j)!=1){
          return(NA)
        } else {
          return(vals_j)
        }
      })
      
      out_df = data.frame(y = rep(NA,nrow(df_j)))
      out_df$y[nresp_vec==1] = yresp_vec_subset
      
    } else {
      
      out_df = df_j
      names(out_df) = "y"
      
    }
    names(df_j)[1] = "y"
    labs_j = sj_df$values
    names(labs_j) =sj_df$labels
    out_df$y =  haven::labelled(out_df$y, 
                                label = sjlabelled::get_label(out_df$y), 
                                labels = labs_j)
    names(out_df)[1] = vars[j]
    return(out_df)
  }) %>% 
    dplyr::bind_cols() %>% 
    dplyr::mutate(ResponseId = df$ResponseId)
  
  for(j in 1:length(vars)){
    if(sum(startsWith(names(df), vars[j]))>0){
      df = df %>% dplyr::select(-starts_with(vars[j]))
    } 
  }
  
  df = df %>% dplyr::left_join(combined_df, by = "ResponseId")
  
  return(df)
  
}


  
  

