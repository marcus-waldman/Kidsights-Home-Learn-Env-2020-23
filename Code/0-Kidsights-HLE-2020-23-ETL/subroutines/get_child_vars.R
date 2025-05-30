get_child_vars<-
  function(i, adult_vars = c("ResponseId", "StartDate", "SQCED", 'SQINC',"SQZIP","SQINTRO","SQ003","SQCMENH")){
    child_vars<-  
      c(adult_vars, 
#        paste0("SQOC", i),
        paste0("SQCARE",i),
        paste0("SQCBD",i,c("_1","_2","_3")), 
        paste0("SQRACE",i,paste0("_",as.character(1:16))), 
        paste0("SQCETH", i),
        paste0("SQSEX",i), 
        paste0("SQCHEALTH",i)
      )
    return(child_vars)
  }