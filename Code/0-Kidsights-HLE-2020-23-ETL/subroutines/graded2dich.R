graded2dich<-function(y, varname = "y"){
  
  require(tidyverse)
  
  
  K = max(y, na.rm = T)+1
  
  names_out = rep("",K)
  for(k in 1:K){
    names_out[k] = paste0(varname,"_",k-1)
  }
  
  
  out <- lapply(1:length(y), function(x){
    ret = rep(NA,K)
    if(!is.na(y[x])){
      ret[seq(1,y[x]+1)] = 1
      ret[-seq(1,y[x]+1)] = 0
    }
    names(ret) = names_out
    return(ret)
  }) %>% dplyr::bind_rows()
  
  
  return(out)

  
}

