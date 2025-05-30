get_cpi99<-function(yr){
  
  
  cpi99 = rep(NA, length(yr))
  cpi99[yr==2023] = mean(c(Jan = 0.56,
                           Feb = 0.56,
                           Mar = 0.56,
                           Apr = 0.55,
                           May = 0.55,
                           Jun = 0.55,
                           Jul = 0.55,
                           Aug = 0.55,
                           Sep = NA,
                           Oct = NA,
                           Nov = NA,
                           Dec = NA), na.rm = T ) #From month 2022 to month 1999 comparisons in https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1.00&year1=202209&year2=199909
  cpi99[yr==2022] = mean(c(Jan = 0.58,
                                Feb = 0.58,
                                Mar = 0.57,
                                Apr = 0.57,
                                May = 0.57,
                                Jun = 0.56,
                                Jul = 0.56,
                                Aug = 0.56,
                                Sep = 0.57,
                                Oct = 0.56,
                                Nov = 0.56,
                                Dec = 0.56), na.rm = T ) #From month 2022 to month 1999 comparisons in https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1.00&year1=202209&year2=199909
  cpi99[yr==2021] = mean(c(Jan = 0.63,
                           Feb = 0.63,
                           Mar = 0.62,
                           Apr = 0.62,
                           May = 0.62,
                           Jun = 0.61,
                           Jul = 0.61,
                           Aug = 0.61,
                           Sep = 0.61,
                           Oct = 0.61,
                           Nov = 0.61,
                           Dec = 0.60), na.rm = T ) #From month 2021 to month 1999 comparisons in https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1.00&year1=202209&year2=199909
  
  #taken from https://cps.ipums.org/cps/cpi99.shtml
  cpi99[yr==2020] = .644
  cpi99[yr==2019] = .652
  cpi99[yr==2018] = .663
  cpi99[yr==2017] = .679
  cpi99[yr==2016] = .694
  return(cpi99)
}