mplus2pars<-function(itemdat,item_names,fit_mplus){
  pars<-
    mirt::mirt(
      data = itemdat %>% dplyr::select(any_of(item_names)),
      model = 1,
      covdata = itemdat %>% dplyr::select(years),
      formula = ~ log(years+.10),
      pars = "values"
    )
  
  a1_mplus = fit_mplus$parameters$unstandardized %>% dplyr::filter(endsWith(paramHeader,"BY"))
  ids_a1 = which(pars$name == "a1")
  for(i in ids_a1){
    pars$value[i] = a1_mplus$est[a1_mplus$param == toupper(pars$item[i])]
  }
  ds_mplus = fit_mplus$parameters$unstandardized %>% dplyr::filter(endsWith(paramHeader,"Thresholds")) %>% 
    dplyr::mutate(item = stringr::str_split_i(param,"\\$",1), 
                  cat = stringr::str_split_i(param,"\\$",2))
  ids_d = which(pars$name %in% c("d", paste0("d",1:5)))
  
  for(i in ids_d){
    name_i = pars$name[i]
    cat_i  = ifelse(name_i!="d", stringr::str_remove_all(name_i,"d") %>% as.numeric(), 1)
    item_i = toupper(pars$item[i])
    pars$value[i] = -ds_mplus$est[ds_mplus$item == item_i & ds_mplus$cat==cat_i]
  }
  
  on_mplus = fit_mplus$parameters$unstandardized %>% dplyr::filter(endsWith(paramHeader,"ON"))
  pars$value[startsWith(pars$name,"F1_log")] = on_mplus$est
  
  return(pars)
}

