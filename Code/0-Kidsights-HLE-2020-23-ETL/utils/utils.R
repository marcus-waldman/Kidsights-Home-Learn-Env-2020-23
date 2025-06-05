get_derived_vars<-function(df_in, path){
  
  df_out = df_in %>% 
    dplyr::mutate(childACES = acedivorce_rc+acedeath_rc+acejail_rc+acedv_rc+aceviolence_rc+acementalill_rc+aceaddict_rc+aceracism_rc,
                  a1_ACES = df_in %>% dplyr::select(starts_with("PACE")) %>% apply(1, "sum"), 
                  ACES_mom = ifelse(a1_parent==1 & a1_female==1, a1_ACES,NA), 
                  ACES_pop = ifelse(a1_parent==1 & a1_female==0, a1_ACES,NA),
                  fciactivities = df_in %>% dplyr::select(starts_with("FCI.D")) %>% apply(1, "sum"),
                  fciactivities = fciactivities - min(fciactivities,na.rm=T),
                  fciactivities_rev = abs(fciactivities - max(fciactivities,na.rm =T)),
                  fcimaterials = df_in %>% dplyr::select(starts_with("FCI.C")) %>% apply(1, "sum"),
                  fcimaterials = fcimaterials - min(fcimaterials, na.rm = T),
                  fcimaterials_rev = abs(fcimaterials - max(fcimaterials, na.rm = T)),
                  a1_depanx = as.numeric((CQFB013+CQFB014+CQFB015+CQFB016)-4) %>% zap_attributes(), 
                  depanx_mom = ifelse(a1_parent==1 & a1_female==1, a1_depanx, NA), 
                  depanx_pop = ifelse(a1_parent==1 & a1_female==0, a1_depanx, NA),
                  # Family size
                  famcount = FQLIVE1_1 + FQLIVE1_2 - 2,
                  famcount = ifelse(famcount<2, 2,famcount),
                  povertyline = povertyline(yr=2022,famsize=famcount, path = path),
                  povertyline_max = povertyline(yr=2022,famsize=7, path = path),
                  povertyline_min = povertyline(yr=2022,famsize=2, path = path),
                  cpi99 = get_cpi99(2022),
                  educ_mom = ifelse(a1_parent==1 & a1_female==1, as.character(a1_educG), NA) %>% char2factor(), 
                  educ_pop = ifelse(a1_parent==1 & a1_female==1, as.character(a1_educG), NA) %>% char2factor(), 
                  wic = 0,
                  wic = ifelse(CQR007_7==1,1,0), 
                  snap = 0, 
                  snap = ifelse(CQR007_5==1,1,0) 
                  
                )
  return(df_out)
}
impute<-function(df_in, path, dat22, M = 2^7, numtree=100, maxit=20){
  t1 = proc.time()
  pb <- txtProgressBar(max = M, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  implist<-foreach(m=1:M, .options.RNG=42, .packages=c("mice","tidyverse","ranger","zipcodeR"), .options.snow = opts) %dorng% {
    source("utils/utils.R")
    subroutines = list.files(paste0(path,"/subroutines"), pattern = ".R", full.names = T)
    for(x in 1:length(subroutines)){source(file = subroutines[x])}
    for(iter in 1:maxit){
      if(iter==1){
        datainit = mice::mice(
          data = df_in,
          method = "rf", ntree = numtree,
          maxit = 1,
          m = 1, remove.collinear = FALSE) %>%
          mice::complete(1) %>%
          get_derived_vars() %>%
          mice::mice(method = "rf", ntree = numtree, maxit = 1, m = 1, remove.collinear = F) %>% 
          mice::complete(1)
      } else {
        datainit = mice::mice(
          data = df_in,
          data.init = datainit,
          method = "rf", ntree = numtree,
          maxit = 1,
          m = 1, remove.collinear = FALSE) %>%
          mice::complete(1) %>%
          get_derived_vars() %>%
          mice::mice(method = "rf", ntree = numtree, maxit = 1, m = 1,  remove.collinear = F) %>% 
          mice::complete(1)
      }
    }
    dfout = datainit %>%
      dplyr::mutate(.imp = m, ResponseId = dat22$ResponseId, zip = dat22$zip) %>%
      dplyr::relocate(.imp,ResponseId,zip) %>%
      dplyr::left_join(dat22 %>% dplyr::select(ResponseId,zKidsight,Kidsight,GEN, dplyr::starts_with("credi_"),"dscore","daz_score","ecdi"), by = "ResponseId")
    return(dfout)
  }
  close(pb)
  cat("\n")
  print(proc.time()-t1)
  return(implist)
}
char2factor<-function(x){
  out=as.factor(x)
  if("White Alone" %in% x){out = relevel(out, ref = "White Alone"); return(out)}
  if("Bachelor's Degree" %in% x){ out = relevel(out, ref = "Bachelor's Degree"); return(out)  }
  if("College Degree" %in% x){ out = relevel(out, ref = "College Degree"); return(out)  }
  return(out)
}
char2ordered<-function(x){
  if("Bachelor's Degree" %in% x){
    out = ordered(x, levels = c("Less than High School Graduate",
                                "High School Graduate (including Equivalency)", 
                                "Some College or Associate's Degree", 
                                "Bachelor's Degree", 
                                "Graduate or Professional Degree")
    )
    return(out)
  }
  return(NULL)
}
get_PACEr<-function(df_in){
  df_out = df_in %>% 
    dplyr::mutate(across(CACE1:CACE10, function(x){x[x>2]<-NA; return(x)})) %>% 
    dplyr::mutate(PACE1r = ifelse(CACE1==2,0,1), 
                  PACE2r = ifelse(CACE2==2,0,1), 
                  PACE3r = ifelse(CACE3==2,0,1), 
                  PACE4r = ifelse(CACE4==2,0,1), 
                  PACE5r = ifelse(CACE5==2,0,1), 
                  PACE6r = ifelse(CACE6==2,0,1), 
                  PACE7r = ifelse(CACE7==2,0,1), 
                  PACE8r = ifelse(CACE8==2,0,1), 
                  PACE9r = ifelse(CACE9==2,0,1), 
                  PACE10r = ifelse(CACE10==2,0,1) 
    ) 
}
get_race<-function(df_in){
  df_out = df_in %>% dplyr::mutate(
    sum_race_acs = 
      as.integer(!is.na(CQR010_1)) + #White
      as.integer(!is.na(CQR010_2)) + #Black
      as.integer(!is.na(CQR010_3)) + #American Indian or Alaskan Native
      as.integer(# Asian or Pacific Islander
        !is.na(CQR010_4) |
          !is.na(CQR010_5) | 
          !is.na(CQR010_6) | 
          !is.na(CQR010_7) | 
          !is.na(CQR010_8) | 
          !is.na(CQR010_9) | 
          !is.na(CQR010_10) | 
          !is.na(CQR010_11) |
          !is.na(CQR010_12) | 
          !is.na(CQR010_13) | 
          !is.na(CQR010_14)   
      ) +
      as.integer( #Some other race
        !is.na(CQR010_15) | 
          !is.na(CQR010_16)
      ),
    white_alone_acs = (sum_race_acs==1 & !is.na(CQR010_1)), 
    black_alone_acs = (sum_race_acs==1 & !is.na(CQR010_2)), 
    native_alone_acs = (sum_race_acs==1 & !is.na(CQR010_3)), 
    asianpi_alone_acs = (is.na(CQR010_1) & is.na(CQR010_2) & is.na(CQR010_3) & is.na(CQR010_15) & is.na(CQR010_16) ) &
      (
        !is.na(CQR010_4) | 
          !is.na(CQR010_5) | 
          !is.na(CQR010_6) | 
          !is.na(CQR010_7) | 
          !is.na(CQR010_8) | 
          !is.na(CQR010_9) | 
          !is.na(CQR010_10) | 
          !is.na(CQR010_11) |
          !is.na(CQR010_12) | 
          !is.na(CQR010_13) | 
          !is.na(CQR010_14)   
      ), 
    someother_alone_acs = (sum_race_acs==1 & (!is.na(CQR010_15) | !is.na(CQR010_16))), 
    twoormore_acs = (sum_race_acs>1), 
    race = NA, 
    race = ifelse(white_alone_acs, "White Alone", race), 
    race = ifelse(black_alone_acs, "Black or African-American Alone", race), 
    race = ifelse(native_alone_acs, "American Indian or Alaskan Native Alone", race), 
    race = ifelse(asianpi_alone_acs, "Asian or Pacific Islander Alone", race), 
    race = ifelse(someother_alone_acs, "Some Other Race", race), 
    race = ifelse(twoormore_acs, "Two or More Races", race)
  )
  return(df_out)
}
get_race_Ghandour<-function(df_in){
  df_out = df_in %>% dplyr::mutate(
    sum_race_acs = 
      as.integer(!is.na(CQR010_1)) + #White
      as.integer(!is.na(CQR010_2)) + #Black
      as.integer(!is.na(CQR010_3)) + #American Indian or Alaskan Native
      as.integer(# Asian or Pacific Islander
        !is.na(CQR010_4) |
          !is.na(CQR010_5) | 
          !is.na(CQR010_6) | 
          !is.na(CQR010_7) | 
          !is.na(CQR010_8) | 
          !is.na(CQR010_9) | 
          !is.na(CQR010_10) | 
          !is.na(CQR010_11) |
          !is.na(CQR010_12) | 
          !is.na(CQR010_13) | 
          !is.na(CQR010_14)   
      ) +
      as.integer( #Some other race
        !is.na(CQR010_15) | 
          !is.na(CQR010_16)
      ),
    white_alone_acs = (sum_race_acs==1 & !is.na(CQR010_1)), 
    black_alone_acs = (sum_race_acs==1 & !is.na(CQR010_2)), 
    native_alone_acs = (sum_race_acs==1 & !is.na(CQR010_3)), 
    asianpi_alone_acs = (is.na(CQR010_1) & is.na(CQR010_2) & is.na(CQR010_3) & is.na(CQR010_15) & is.na(CQR010_16) ) &
      (
        !is.na(CQR010_4) | 
          !is.na(CQR010_5) | 
          !is.na(CQR010_6) | 
          !is.na(CQR010_7) | 
          !is.na(CQR010_8) | 
          !is.na(CQR010_9) | 
          !is.na(CQR010_10) | 
          !is.na(CQR010_11) |
          !is.na(CQR010_12) | 
          !is.na(CQR010_13) | 
          !is.na(CQR010_14)   
      ), 
    someother_alone_acs = (sum_race_acs==1 & (!is.na(CQR010_15) | !is.na(CQR010_16))), 
    twoormore_acs = (sum_race_acs>1), 
    raceG = NA, 
    raceG = ifelse(hisp==1, "Hispanic", raceG),
    raceG = ifelse(white_alone_acs & hisp==0, "White, non-Hisp.", raceG), 
    raceG = ifelse(black_alone_acs & hisp==0, "Black or African-American, non-Hisp.", raceG), 
    raceG = ifelse(native_alone_acs & hisp==0, "American Indian or Alaskan Native, non-Hisp.", raceG), 
    raceG = ifelse(asianpi_alone_acs & hisp==0, "Asian or Pacific Islander, non-Hisp.", raceG), 
    raceG = ifelse(someother_alone_acs & hisp==0, "Some Other Race, non-Hisp.", raceG), 
    raceG = ifelse(twoormore_acs & hisp==0, "Two or More, non-Hisp.", raceG)
    )
  return(df_out)
}
get_educ<-function(df_in, source = "NE22"){
  

  if(source %in% c("NE20","NE22")){
    df_out <- df_in %>% dplyr::mutate(
      a1_educ = NA, 
      a1_educ = ifelse(as.integer(CQR004)<3, "Less than High School Graduate", a1_educ), 
      a1_educ = ifelse(as.integer(CQR004)==3, "High School Graduate (including Equivalency)", a1_educ), 
      a1_educ = ifelse(as.integer(CQR004)>3 & as.integer(CQR004)<7, "Some College or Associate's Degree", a1_educ),
      a1_educ = ifelse(as.integer(CQR004)==7, "Bachelor's Degree",a1_educ), 
      a1_educ = ifelse(as.integer(CQR004)>7, "Graduate or Professional Degree", a1_educ)
    )
  }

  return(df_out)
}
get_educ_Ghandour<-function(df_in){
  df_out = df_in %>% dplyr::mutate(
    a1_educG = ifelse(a1_educ %in% c("Bachelor's Degree", "Graduate or Professional Degree"),"College Degree", as.character(a1_educ)) %>% char2factor()
  )
  return(df_out)
}
get_misc<-function(df_in){
  df_out = df_in %>% dplyr::mutate(
    troublebasics = zap_attributes(CQFA005-1), # Same as ACE1
    foodinsuff = plyr::mapvalues(
      CQFA006 %>% zap_attributes() %>% as.numeric(), 
      from = c(1,2,4,5), 
      to = c("Always afford good nutrious meal","Always afford but not always kinds of food","Sometimes/Often afford","Sometimes/Often afford")
    ) %>% as.factor(),
    handles_demands = as.integer(CQFB003<3) %>% zap_attributes(), 
    emotsupport = as.integer(CQFA010==1) %>% zap_attributes(), 
    care10hrs =as.integer(care10hrs_rc==1) %>% zap_attributes(), 
    years = days/365.25 %>% zap_attributes(), 
    childcare = as.integer(CQFB007==1) %>% zap_attributes()
  )
  return(df_out)
}
recode_FCI<-function(df_in){
  df_out = df_in  %>% 
    dplyr::mutate(
      across(FCI.D.1:FCI.D.11, function(x){x[x==3] = NA; as.integer(x==1)}), 
      across(FCI.C.1:FCI.C.11, function(x){x[x==3] = NA; as.integer(x==1)}) 
    )
  return(df_out)
}
recode_CQR007<-function(df_in){
  df_out = df_in %>% 
    dplyr::mutate(
      across(CQR007_1:CQR007_7, function(x){x=zap_attributes(x); x[is.na(x)] = 0; return(x)})
    )
  return(df_out)
}



make_design_weights<-function(implist, Bootstraps = 16, M = 128, poolfileloc = "C:/Users/marcu/Dropbox/UNMC/phase-2/KidsightsDashboard/DashboardNE22/KidsightDashboardNE22/data/implist_pool_of_NE_microdata.rds"){
    library(tidyverse)
    library(srvyr)
    library(svrep)
    library(MCMCpack)
    
    pool_list = readr::read_rds(file = poolfileloc)
    
    #print(Bootstraps)
    #print(M)
    cl = parallel::makeCluster(16)
    parallel::clusterExport(cl, c("implist", "Bootstraps","raking_loss_p", "to_acs","M", "pool_list"))
    wgts_list<-pbapply::pblapply(1:M, function(m){
      
      library(tidyverse)
      library(srvyr)
      library(svrep)
      
      unmc_m = implist[[m]] %>% 
        dplyr::mutate( 
          sc_age_years = floor(years), 
          fpl_i1 = round(100*inc99/(cpi99*povertyline),0), 
          fpl_i1 = ifelse(fpl_i1>=400, 400, fpl_i1),
          fpl_i1 = ifelse(fpl_i1<50, 50, fpl_i1),
          native = as.integer(raceG=="American Indian or Alaskan Native, non-Hisp." ),
          api = as.integer(raceG=="Asian or Pacific Islander, non-Hisp."), 
          black = as.integer(raceG=="Black or African-American, non-Hisp." ), 
          other = as.integer(raceG=="Some Other Race, non-Hisp."), 
          twoplus = as.integer(raceG=="Two or More, non-Hisp."), 
          white = as.integer(raceG=="White, non-Hisp."), 
          nohisp = 1-hisp, 
          fpl0 = as.integer(fpl_i1<100), 
          fpl1 = as.integer(fpl_i1>=100 & fpl_i1<200), 
          fpl2 = as.integer(fpl_i1>=200 & fpl_i1<300), 
          fpl3 = as.integer(fpl_i1>=300 & fpl_i1<400), 
          fpl4 = as.integer(fpl_i1>=400), 
          ba = as.integer(educ_mom=="College Degree"), 
          hs = as.integer(educ_mom=="High School Graduate (including Equivalency)"), 
          lths = as.integer(educ_mom=="Less than High School Graduate"), 
          aa = as.integer(educ_mom=="Some College or Associate's Degree"), 
          source = "UNMC", 
          wgt = 1)
      
      acs_m = pool_list[[m]] %>% 
        dplyr::filter(source == "ACS") %>% 
        to_acs() 
      
      outlist_m <- lapply(0:5, function(yr_x){
        
        # Get imputation-specific weights (no bootstrap)
        acs_mx = acs_m %>% 
          dplyr::filter(sc_age_years == yr_x) %>% 
          dplyr::select(stratum,serial, perwt, metro, sc_age_years, hisp:fpl4)    
        
        targets_acs_mx = acs_mx %>% 
          pivot_longer(col = hisp:fpl4, names_to = "variable", values_to = "value") %>% 
          srvyr::as_survey_design(strata = stratum, weights = perwt, id = serial) %>%  
          dplyr::group_by(sc_age_years, variable) %>% 
          dplyr::summarise(pcts = srvyr::survey_mean(value, proportion = T, prop_method = "beta"))
        
        unmc_mx = unmc_m %>% 
          dplyr::filter(sc_age_years == yr_x) %>% 
          dplyr::select(ResponseId, dplyr::all_of(targets_acs_mx$variable))
        n_mx = nrow(unmc_mx)
        
        
        optim_mx = optim(
          par = rep(1,nrow(unmc_mx)), 
          fn = raking_loss_p, 
          dat = unmc_mx %>% dplyr::select(-ResponseId), 
          target=targets_acs_mx %>%  purrr::pluck("pcts"),
          se=targets_acs_mx %>% purrr::pluck("pcts_se"),
          lower = .1,
          upper = 10,
          method = "L-BFGS-B", 
          control = list(maxit = 1E4) 
        )
        
        wgt_mx = optim_mx$par
        
        out_mx = unmc_mx %>% 
          dplyr::mutate(.imp = m, sc_age_years = yr_x)  %>% 
          dplyr::select(.imp, ResponseId, sc_age_years) %>% 
          dplyr::mutate(wgt = wgt_mx/mean(wgt_mx))
        
        acs_bperwt_mx = acs_mx %>%
          dplyr::filter(sc_age_years == yr_x) %>% 
          srvyr::as_survey_design(strata = stratum, weights = perwt, id = serial) %>%  
          svrep::as_bootstrap_design(replicates = Bootstraps) %>% 
          weights(type = "analysis")
        
        tmplist<-lapply(1:Bootstraps, function(b){

          targets_acs_mxb = acs_mx %>% 
            dplyr::mutate(perwt = acs_bperwt_mx[,b]) %>% 
            dplyr::select(stratum,serial, perwt, sc_age_years, hisp:fpl4) %>% 
            pivot_longer(col = hisp:fpl4, names_to = "variable", values_to = "value") %>% 
            srvyr::as_survey_design(strata = stratum, weights = perwt, id = serial) %>%  
            dplyr::group_by(sc_age_years, variable) %>% 
            dplyr::summarise(pcts = weighted.mean(value, w = perwt)) %>% 
            dplyr::left_join(targets_acs_mx %>% dplyr::select(sc_age_years, variable, pcts_se), by = c("sc_age_years","variable"))
          
          
          bbw_b = MCMCpack::rdirichlet(1, rep(1,n_mx)) %>% as.vector()
          optim_mxb = optim(
            par =wgt_mx, 
            fn = raking_loss_p, 
            dat = unmc_mx %>% dplyr::select(-ResponseId), 
            target=targets_acs_mxb %>% purrr::pluck("pcts"),
            se=targets_acs_mxb %>% purrr::pluck("pcts_se"),
            bbw = bbw_b,
            lower = .1,
            upper = 10,
            method = "L-BFGS-B", 
            control = list(maxit = 1E4) 
          )
          
          
          wgts_mxb = optim_mxb$par*bbw_b
        })

        for(b in 1:Bootstraps){
          wgts_mxb = tmplist[[b]]
          out_mx = out_mx %>% dplyr::mutate(wgtb = wgts_mxb/mean(wgts_mxb))
          names(out_mx)[ncol(out_mx)] = paste0("wgt", b)
        }
        
        
        return(out_mx)
      })
      
      ret_m = outlist_m %>% dplyr::bind_rows()
      
      return(ret_m)
      
    }, cl=cl)
    parallel::stopCluster(cl)
    
    
    readr::write_rds(wgts_list, file = "data/design_weights.rds")
    return(wgts_list)
    
    
}
psy_scores<-function(path,...){
  psy_list = readr::read_rds(file.path(path,"/Sixpence/psychosocial_calibration_mirt_V2.rds"))
  mrw2RId = readr::read_rds(file = paste0(intermediate_path,"/mrwid2RId.RDS"))
  grm_psy =  mirt::mirt(data= psy_list %>% purrr::pluck("data") %>% dplyr::select(starts_with("PS")) %>% dplyr::select(-"PS033"), technical = list(NCYCLES = 2000))
  EAP_psy = mirt::fscores(grm_psy) %>% as.numeric()
  tmpdat = data.frame(mrwid = psy_list$data$mrwid, PSY = EAP_psy) %>% dplyr::left_join(mrw2RId, by = "mrwid") %>% 
    dplyr::filter(startsWith(ResponseId,"R_"))
  return(tmpdat)
}
bind2implist<-function(list_in,df_in){
  list_out = list(NULL)
  for(m in 1:length(list_in)){
    list_out[[m]] = list_in[[m]] %>% dplyr::left_join(df_in, by = "ResponseId")
  }
  return(list_out)
}
raking_loss_p<-function(w,dat,target, se = NULL, bbw=rep(1,length(w))){
  xtilde = apply(dat,2,"weighted.mean", w = w*bbw)
  delta = xtilde-target
  if(is.null(se)){
    objective  = sum(delta^2)
  } else {
    objective = sum( (delta/se)^2 )
  }
  penalty = 10*(mean(w)-1)^2
  return(objective+penalty)
}


to_acs <- function(df){
  
  out = df %>% 
    dplyr::mutate(recnum = 1:n()) %>% 
    dplyr::relocate(recnum) %>% 
    dplyr::mutate(
      native = as.integer(race6=="Native" & hisp == "0"), 
      api = as.integer(race6=="API"  & hisp == "0"), 
      black = as.integer(race6=="Black"  & hisp == "0"),
      other = as.integer(race6=="Other"  & hisp == "0"),
      twoplus = as.integer(race6=="TwoPlus"  & hisp == "0"), 
      white = as.integer(race6=="White" & hisp == "0"), 
      hisp = as.integer(hisp=="1"), 
      nohisp = as.integer(hisp == "0"), 
      fpl0 = as.integer(fpl_i1<100), 
      fpl1 = as.integer(fpl_i1>=100 & fpl_i1<200), 
      fpl2 = as.integer(fpl_i1>=200 & fpl_i1<300), 
      fpl3 = as.integer(fpl_i1>=300 & fpl_i1<400), 
      fpl4 = as.integer(fpl_i1>=400),
      lths = as.integer(educ_mom=="Less than High School Graduate"), 
      hs = as.integer(educ_mom=="High School Graduate (including Equivalency)"), 
      aa = as.integer(educ_mom=="Some College or Associate's Degree"),
      ba = as.integer(educ_mom=="Bachelor's Degree" | educ_mom=="Graduate or Professional Degree"), 
      metro = as.integer(metro), 
      female = as.integer(female==1), 
      mu_years = 3,
      sig_years = sqrt( 6^2/12 ), 
      sc_age_years = ifelse(source == "ACS", floor(syears*sig_years + mu_years), sc_age_years)
    ) %>% 
    dplyr::mutate(stratum = paste(source,stratum, sep = "-")) %>% 
    dplyr::select(source, year, stratum, serial, pernum, perwt, wgt, perwt, metro, sc_age_years, hisp, nohisp, native, api, black, other, twoplus, white, ba, hs, lths, aa, fpl0:fpl4) 
  
  return(out)
  
}

ETL_agreed_vars<-function(df_in){
  
  agreed_vars<-
    c( ".imp","ResponseId",
      #Outcomes
      "Kidsight", "PSY", 
      "dscore", 
      paste0("credi_",c("COG","LANG","MOT", "SEM", "OVERALL")), 
      "ecdi", 
      paste0("nom",c("learn","phys","sem","sreg")),
      #Mediators
      # Family Context
      "foodinsuff", 
      "troublebasics", 
      "emotsupport",
      "care10hrs", 
      "ACES_mom", 
      "ACES_pop",
      "depanx_mom", 
      "depanx_pop",
      "handles_demands",
      "fciactivities", 
      "fcimaterials", 
      #Child trauma
      "childACES",
      #Controls, 
      #Child demographic
      "years", "female", "race",  
      # Family socioeconomic
      "educ_mom", "educ_pop", 
      # Geographic 
      "zip","lat","lng","metro","smedhhinc","slpopdense",
      # Responder type
      "a1_female",
      # Others
      "famcount","a1_educG", "a1_depanx", "a1_ACES", "childcare", 
      # Services
      "snap","wic", 
      ##Federal poverty line
      "fpl_cat",
      #Child demographic
      "years", "female", "raceG"
      
  )
  
  
  
  df_out = df_in %>% 
    dplyr::mutate(
      
      fpl_i1 = round(100*inc99/(cpi99*povertyline),0),
      fpl_cat = NA, 
      fpl_cat = ifelse(fpl_i1<100, 0, fpl_cat),
      fpl_cat = ifelse(fpl_i1>=100 & fpl_i1<200,1,fpl_cat), 
      fpl_cat = ifelse(fpl_i1>=200 & fpl_i1<300,2,fpl_cat), 
      fpl_cat = ifelse(fpl_i1>=300 & fpl_i1<400,3,fpl_cat), 
      fpl_cat = ifelse(fpl_i1>=400,4,fpl_cat),
      
      foodinsuff = as.integer(foodinsuff)
    ) %>% 
    dplyr::select(dplyr::any_of(agreed_vars))
  
  return(df_out )
}

