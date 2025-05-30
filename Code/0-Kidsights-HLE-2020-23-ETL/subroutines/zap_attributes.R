zap_attributes=function(x){
  require(haven)
  if(is.character(x)){x = haven::zap_empty(x)}
  x_zapped = x %>% haven::zap_labels() %>% haven::zap_label()
  return(x_zapped)
}