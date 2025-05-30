read_geocorr<-function(geography = "state", path = "C:/Users/marcu/Dropbox/UNMC/phase-2/data-files/source-files/GEOCORR", add_label = F){
  
  # Geography: Target geography from zipcode
    # bestschooldistrict - School district
    # cbsa - Core based statistic area and type (micro vvs. metro vs. none)
    # censustract20 - 2020 Census tract
    # county - County
    # puma20 - 2020 Census public use microareas 
    # sdll22 - 2022 State legislative districts (lower chamber)
    # sdlu22 - 2022 State legislative districuts (upper chamber)
    # urbanruralportion - Urban vs. rural 
    # uscongress - 118 US Congress districts
    # state - State
    # placesizecat = Place size category
    # place
    # countysizecat
  foo = readr::read_csv(file = paste0(path,"/zip2",geography,".csv"))
  
  res = foo[-1,] %>% dplyr::filter(!is.na(zcta))
  labsdf = data.frame(var = names(res), label = foo[1,] %>% as.character()) %>% dplyr::mutate(class = ifelse(var=="afact", "numeric", "character"))
  print(labsdf)
  
  res$afact = as.numeric(res$afact)
  
  if(add_label){res = res %>% sjlabelled::set_label(labsdf$label)}
  
  return(res)
  
}
