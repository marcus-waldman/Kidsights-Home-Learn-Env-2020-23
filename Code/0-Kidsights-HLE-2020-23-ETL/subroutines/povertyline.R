
povertyline<-function(yr, famsize, guidelines=readr::read_rds(file = "C:/Users/marcu/Dropbox/UNMC/phase-2/data-files/intermediate-files/federal_poverty_guidelines_5June2024.rds")){
  
  #yr - Numberic vector 
  #famsize = Numeric vector of family size
  #guidelines - List with elements: 
  #   guideline$thresholds = thresholds by year and family size (famcounnt)
  #  guidelines@
  # Note guidelines last updated 5June2024. See: https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines
  # NOTE not accurate for AK or HI
  foo = data.frame(year=yr, famcount=famsize) %>% 
    dplyr::mutate(above8count = ifelse(famsize>8,famsize-8,0), 
                  famcount = ifelse(famsize>8, 8,famsize)) %>% 
    dplyr::left_join(guidelines$above8, by = c("year")) %>% 
    dplyr::left_join(guidelines$thresholds, by = c("year","famcount")) %>% 
    dplyr::mutate(fpl100 = fpl100+above8count*above8each) 
  
  
  return(foo$fpl100)
  
}
