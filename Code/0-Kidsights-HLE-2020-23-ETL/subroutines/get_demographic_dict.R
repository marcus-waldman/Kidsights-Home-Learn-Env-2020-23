get_demographic_dict<-function(cahmi, acs){

  require(tidyverse)
  
  data.dict = data.frame(var = names(cahmi),
                         var_type = "numeric",
                         label = "", 
                         in_cahmi = NA, 
                         in_acs = NA, 
                         impute_block = "", 
                         impute_dashboard = F)
  
  combined_df = cahmi %>% bind_rows(acs)
  
  for(j in 1:ncol(cahmi)){
    
    lab_j = sjlabelled::get_label(cahmi[,j])
    if(!is.null(lab_j)){
      data.dict$label[j] = as.character(lab_j)
    }
    
    var_j = names(cahmi)[j]
    
    nvals_cahmi = length(table(cahmi[,j], useNA = "always"))
    nvals_acs = length(table(acs[,j], useNA = "always"))
    nvals_combined = length(table(combined_df[,j], useNA = "always"))
    
    data.dict$in_cahmi[j] = (nvals_cahmi>1)
    data.dict$in_acs[j] = (nvals_acs>1)
    
    if(nvals_cahmi>1 & nvals_acs>1 & nvals_combined>2){
      data.dict$impute_block[j] = "ACS + CAHMI"
    }
    if(nvals_cahmi<=1 & nvals_acs>1){
      data.dict$impute_block[j] = "ACS"
    }
    if(nvals_cahmi>1 & nvals_acs<=1){
      data.dict$impute_block[j] = "CAHMI"
    }
    
    if(is.character(cahmi[,j])){
      data.dict$var_type[j] = "character"
    }
  

  } #end for j
  
  data.dict$impute_block[endsWith(data.dict$var,"id")] = ""
  data.dict$impute_block[startsWith(data.dict$var,"a2_")] = ""
  data.dict$impute_dashboard[ data.dict$var %in%  c("puma","rural","pctmetro","inc99","famcount","fpl_i1","a1_edu4","a1_menthealth","sc_age_years","female","race4_char","hisp","black_any","white_any", "chhealth","care10hrs_rc")] = 1

  
  return(data.dict)
  
}
