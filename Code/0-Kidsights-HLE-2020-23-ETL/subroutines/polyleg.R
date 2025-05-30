polyleg<-function(z,p){
  N = length(z)
  if(p==0){out = rep(1,N)}
  if(p==1){out = z}
  if(p==2){out = .5*(3*z^2 -1)}
  if(p==3){out = .5*(5*z^3-3*z)}
  if(p==4){out = (1/8)*(35*z^4-30*z^2+3)}
  if(p>4){warning("Only up to degree 4 supported."); out = rep(NA,N)}
  return(out)
}