strata_filter<-function(x,what){
  if(is.na(what)){
    return(rep(T,length(x)))
  }
  if(what == "gt_fpl_185"){
    return(x>185)
  }
  if(what == "lt_fpl_185"){
    return(x<=185)
  }
  if(what == "hisp"){
    return(x=="Hispanic")
  }
  if(what == "white"){
    return(startsWith(x,"White"))
  }
  if(what == "Black"){
    return(startsWith(x,"Black"))
  }
  if(what=="college"){
    return(x==3)
  }
  if(what == "no college"){
    return(x<3)
  }
  if (what == "rural"){
    return(x == 1)
  }
  if(what == "urban"){
    return(x == 0)
  }
}