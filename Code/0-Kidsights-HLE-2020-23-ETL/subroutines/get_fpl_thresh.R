get_fpl_thresh<-function(nfam, year = 2022){
  fpl_thresh = NA
  if(year==2022){
    # Taken from https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2021-poverty-guidelines
    fpl_thresh_df = data.frame(famcount = 1:8, 
                               fpl_thresh = c( 12880.00,
                                               17420.00,
                                               21960.00,
                                               26500.00,
                                               31040.00,
                                               35580.00,
                                               40120.00,
                                               44660.00)
    )
    if(nfam<=8){fpl_thresh = fpl_thresh_df$fpl_thresh[nfam]}
    if(nfam>8){fpl_thresh = fpl_thresh_df$fpl_thresh[8]+(nfam-8)*4540}
  } else {
    error("Have not coded federal poverty lines for this year")
  }
  return(fpl_thresh)
}
