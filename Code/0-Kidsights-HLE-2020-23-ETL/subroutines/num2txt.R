num2txt<-function(x){
  x[is.nan(x)] = NA
  x[!is.na(x)] = round(x[!is.na(x)],2)
  x = as.character(x)
  x[is.na(x)] = "-"
  return(x)
}
