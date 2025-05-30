get_stars<-function(p){
  out_vec = rep("",length(p))
  out_vec[p<.05] = "*"
  out_vec[p<.01] = "**"
  out_vec[p<.001] = "***"
  return(out_vec)
}

