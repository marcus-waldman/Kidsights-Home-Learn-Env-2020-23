neglogl<-function(theta,df_long,mu,sigma){
  
  df_long = df_long %>% 
    dplyr::mutate(pr_y = ifelse(delta_y==-Inf,1,plogis(alpha*(theta-delta_y)))) %>% 
    dplyr::mutate(pr_yp1 = ifelse(delta_yp1==Inf, 0, plogis(alpha*(theta-delta_yp1)))) %>% 
    dplyr::mutate(pr = pr_y-pr_yp1)
  
  df_long$pr[df_long$pr<sqrt(.Machine$double.eps)] = sqrt(.Machine$double.eps)
  
  return( -sum(log(df_long$pr)) + .5*( (theta-mu)/sigma )^2 )
}
