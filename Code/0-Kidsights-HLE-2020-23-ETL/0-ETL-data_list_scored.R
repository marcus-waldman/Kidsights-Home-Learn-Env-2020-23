      rm(list = ls())
      
      ##### Preliminaries ####
      #Load libraryd packages
      library(tidyverse)
      library(parallel)
      library(doParallel)
      library(pbapply)
      library(mirt)
      library(ranger)
      library(gamlss)
      library(flexmet)
      library(mice)
      library(ranger)
      library(mboost)
      library(ggthemes)
      library(ggarchery)
      library(credi) #devtools::install_github("marcus-waldman/credi")

      
      

      # Set working directories
      root = "C:/Users/waldmanm"
             #"C:/Users/marcu"
             #"D:/"
      path =  paste0(root,"/Dropbox/UNMC/phase-2")
      intermediate_path = paste0(path,"/data-files/intermediate-files")
      github_path = paste0(root,"/git-repositories/Kidsights-Home-Learn-Env-2020-23/Code/0-Kidsights-HLE-2020-23-ETL")
      onedrive_phase2_data_path = file.path(root,"University of Nebraska Medical Center", "Kidsights Data - Documents", "General", "Phase 2 Data")
      
      # Source in functions and subroutines 
      subroutines = list.files(paste0(github_path,"/subroutines"), pattern = ".R", full.names = T)
      for(x in 1:length(subroutines)){source(file = subroutines[x])}
      
      # # Identify names of files that continually get updated
      SCREENER_filename =  "Nebraska+Child+Development+Study-+Phase+2+SCREENER_February+13,+2023_14.22"
      SURVEY_filename = "Nebraska+Child+Development+Study-+Phase+2+SURVEY_February+13,+2023_14.21"
      set_ineligible_FILENAME = "set_ineligible_22feb2023.csv"
      set_eligible_FILENAME = "set_eligible_22feb2023.csv"
      
      # Preallocate a list for saving information on observations made ineligible.
      made_inelig= list(NULL)
      mix = 0
      
      # Preallocate a list for saving information on items that are removed
      removed_items = list(NULL)
      rix = 0
       
      # ##### 2022-23: Load in, wrangle, and evaluate for veracity/eligibility the raw data ######
      
      # NSCH data (only used as a reference dataset)
      cahmi_2016_2020 = readr::read_rds(file = paste0(path, "/data-files/intermediate-files/cahmi_2016_2020.RDS")) %>%
        dplyr::filter(fipsst == 31) %>%
        recode_cahmi(path = path, cahmi_2016_2020 = .)
      
      # # Load in zipcodeR_tigris
      zipcodeR_tigris = readRDS(paste0(path,"/data-files/intermediate-files/zipcodeR_tigris.RDS"))
      
      # Load in the KH acceptable zip codes
      zipcodes_df = get_KH_acceptable_zipcodes(onedrive_phase2_data_path)
      
      # Load in the latest SCREENER pull
      SCREENER = haven::read_sav(file = paste0(path,"/data-files/source-files/Qualtrics/",SCREENER_filename,".sav"))
      screener_df = SCREENER
      
      # Load in the latest SURVEY pull
      SURVEY = haven::read_sav(file = paste0(path,"/data-files/source-files/Qualtrics/",SURVEY_filename,".sav"))
      survey_df = SURVEY %>%
        recode_qualtrics_survey(reference_df = cahmi_2016_2020, zipcodeR_tigris = zipcodeR_tigris) %>%
        recode_items(intermed_path = intermediate_path)
      elig_list = readr::read_rds(file = paste0(intermediate_path, "/elig_list.rds"))
      
      
      # # Get a raw dataset, restrict to English speaking only
      set_ineligible = readr::read_csv(file = paste0(path,"/data-files/source-files/Qualtrics/", set_ineligible_FILENAME))
      set_eligible = readr::read_csv(file = paste0(path,"/data-files/source-files/Qualtrics/", set_eligible_FILENAME))
      raw22 = elig_list$survey %>%
        handvet_set_eligible(ids = set_eligible$ResponseId) %>%
        handvet_set_ineligible(ids = set_ineligible$ResponseId) 
      mix = mix + 1
      made_inelig[[mix]] = data.frame(N = sum(raw22$eligible==0), Reason = "2022: Did not pass eligibility/cybersecurity screening")
      raw22 = raw22 %>% dplyr::filter(eligible==1)
      
      
      
      spanish_RIds = raw22 %>% dplyr::filter(UserLanguage == "ES-ES") %>% dplyr::select("ResponseId")
      raw22_en_es = raw22 %>% dplyr::filter(eligible == 1)
      
      raw22 = raw22 %>% 
        dplyr::mutate(eligible = ifelse(UserLanguage!="EN", 0, eligible))
      
      mix= mix+1
      made_inelig[[mix]] = data.frame(N = sum(raw22$eligible==0),
                                                     reason = "2022-2023: Non-English survey administration.")
      

      
      raw22 = raw22 %>% dplyr::filter(eligible == 1)
      
      
      
      # # #####  2022-23: Construct a pre-analytic dataset (items + recoded criterion) ####
       lex_xwalk = readr::read_csv(file = paste0(intermediate_path,"/items_lex_crosswalk.csv"), show_col_types = F)
        # Just to be safe so that we are only using lex_kidsight24, remove out  lex_kidsight column from before
        lex_xwalk = lex_xwalk %>% dplyr::select(-lex_kidsight)
      
      dat22 = raw22 %>%
       # dplyr::select(eligible, ResponseId,days,foodstamp_rc:a1_parent, female, race4_char, acedivorce_rc:care10hrs_rc, inc99, rural, CQFB013:CQFB016, all_of(lex_xwalk$lex_ne22), any_of(vars_impute)) %>%
        dplyr::mutate(
          mrwid = -seq(1,nrow(.)),
        ) %>%
        #dplyr::select(-medicare_rc, -a1_menthealth, -a1_physhealth) %>%
        dplyr::relocate(mrwid)
      
      mrwid2RId = dat22 %>% dplyr::select(mrwid, ResponseId)
      #readr::write_rds(mrwid2RId, file = paste0(intermediate_path,"/mrwid2RId.RDS"))
      
      
      # Load in the 2020 dat
      dat20 = read_rds(file = paste0(path,"/data-files/source-files/UNMC-2020/to-abbie-18Aug2021.rds")) %>%
        dplyr::filter(pick1==1, eligible==1) %>% 
        dplyr::mutate(female = c_female)
      names(dat20) = plyr::mapvalues(names(dat20), from = lex_xwalk$lex_ne20, to = lex_xwalk$lex_ne22)
      dat20 = dat20 %>% 
        recode_unmc2020(reference_df = dat22) %>%
        dplyr::mutate(female = c_female) %>% 
        dplyr::select(eligible, any_of(names(dat22))) %>%
        dplyr::mutate(eligible = ifelse(inc99 == max(inc99, na.rm = T), 0, eligible))
      
      
      # Load in the Rapid Dat
      rapid23 = haven::read_sav(file = paste0(root,"/Dropbox/UNMC/ECD Rapid/Data/source-files/23aug2023/RAPID-KidSights/RAPID-KidSights_2023.8.22.sav"))
      rapid23 = rapid23 %>% 
        dplyr::mutate(mrwid = 990000+ 1:nrow(.)) %>%
        dplyr::relocate(mrwid)
      
      
      mix= mix+1
      made_inelig[[mix]] = data.frame(N = sum(dat20$eligible==0),
                                      reason = paste0("2020-2021: Household income way too high $", max(dat20$inc99/get_cpi99(2022))))
      
      dat20 = dat20 %>% dplyr::filter(eligible==1)
      
      # dat = dat22 %>% zap_attributes() %>%dplyr::mutate(across(where(is.ordered), function(x){as.integer(x)-1})) %>%
      #   dplyr::bind_rows(dat20 %>% zap_attributes() %>% dplyr::mutate(across(where(is.ordered), function(x){as.integer(x)-1}))) #%>%
      #   dplyr::mutate(phase2 = ifelse(mrwid<0,1,0),
      #                 PHQ2a = as.integer(CQFB013),
      #                 PHQ2b = as.integer(CQFB014),
      #                 GAD2a = as.integer(CQFB015),
      #                 GAD2b = as.integer(CQFB016),
      #                 eligible = 1) %>%
      #   dplyr::relocate(eligible, mrwid, ResponseId, days, female, phase2:GAD2b) %>%
      #   dplyr::select(-starts_with("CQFB")) %>%
      #   get_concurrent_scores(intermediate_path = intermediate_path)
      # saveRDS(dat, file = paste0(intermediate_path,"/dat.rds"))
      dat = readr::read_rds(file = paste0(intermediate_path,"/dat.rds"))
      
      
      # lets add rapid to this dat 
      itemdat_rapid23 = readr::read_rds(file = paste0(intermediate_path, "/Jun24/itemdat_rapid23.rds")) %>% 
        dplyr::mutate(days = years*365.25, eligible = 1) %>% 
        dplyr::select(-source, -years)
      dat = dat %>% dplyr::bind_rows(itemdat_rapid23) %>% dplyr::select(-(EAT:ecdi_eps))
      
      
      # let's add credi scores
      credi = get_credi_scores(input_df = dat, idvar = "mrwid", intermediate_path = intermediate_path)
      dat = dat %>% dplyr::left_join(credi,by = "mrwid")
      
      
      # let's add ecdi scores
      ecdi = ecdi_fscores(dat, idvar = "mrwid", intermediate_path = intermediate_path)
      dat = dat %>% dplyr::left_join(ecdi, by = "mrwid")

      
      # let's add nom fscores
      nom = nom_fscores(dat, idvar = "mrwid")
      dat = dat %>% dplyr::left_join(nom, by = "mrwid")
      
      
      # Get rid of any observations with no information on child's age
      dat$eligible[is.na(dat$days)] = 0
      mix= mix+1
      made_inelig[[mix]] = data.frame(N = sum(dat$eligible==0, na.rm = T), 
                                      reason = "No information on child's age", 
                                      N_2019 = sum(dat$eligible==0 & dat$mrwid>0, na.rm = T), 
                                      N_2022 = sum(dat$eligible==0 & dat$mrwid<0, na.rm = T))
      dat = dat %>% dplyr::filter(eligible == 1)
      
      
 
      
      # Look for clear outliers with credi
      foo =  dat %>% dplyr::select(mrwid,credi_OVERALL, days) %>% na.omit()
      fit_foo = gamlss(credi_OVERALL~pbm(days), data = foo)
      foo$credi_OVERALL_eps = fit_foo$residuals
      
      #ggplot(foo, aes(x = days, y = credi_OVERALL_eps)) + geom_point()
      dat = dat %>% dplyr::left_join(foo %>% dplyr::select(mrwid, credi_OVERALL_eps), by = "mrwid")
      sdO = dat %>% dplyr::filter(is.finite(credi_OVERALL_eps)) %>% purrr::pluck("credi_OVERALL_eps") %>% sd()
      dat = dat %>% dplyr::mutate(credi_OVERALL_eps = credi_OVERALL_eps/sdO)
      dat$credi_OVERALL_eps[is.infinite(dat$credi_OVERALL_eps)] = NA
      ggplot(dat, aes(x = days, y = credi_OVERALL_eps)) + geom_point() + stat_smooth() # clearly outliers
      
      # Make ineligible any observation with age-adjusted CREDI scores greter than 3.5
      dat$eligible[dat$credi_OVERALL_eps>3.5] = 0
      mix= mix+1
      made_inelig[[mix]] = data.frame(N = sum(dat$eligible==0, na.rm = T), 
                                      reason = "CREDI standardized score greater than 3.5SD ", 
                                      N_2019 = sum(dat$eligible==0 & dat$mrwid>0 & dat$mrwid<990000, na.rm = T), 
                                      N_2022 = sum(dat$eligible==0 & dat$mrwid<0, na.rm = T), 
                                      N_2023 = sum(dat$eligible==0 & dat$mrwid>=990000, na.rm = T)
                                      )
      dat = dat %>% dplyr::filter(eligible == 1)
      
      # Make ineligible any observation with age adjusted CREDI scores less than -5
      dat$eligible[dat$credi_OVERALL_eps< (-5)] = 0
      mix= mix+1
      made_inelig[[mix]] = data.frame(N = sum(dat$eligible==0, na.rm = T), 
                                      reason = "CREDI standardized score less than -5 SD ", 
                                      N_2019 = sum(dat$eligible==0 & dat$mrwid>0, na.rm = T), 
                                      N_2022 = sum(dat$eligible==0 & dat$mrwid<0, na.rm = T), 
                                      N_2023 = sum(dat$eligible==0 & dat$mrwid>=990000, na.rm = T)
                                      )
      dat = dat %>% dplyr::filter(eligible == 1)

      
      
      # Look for ECDI outlier
      foo =  dat %>% dplyr::select(mrwid,ecdi, days) %>% na.omit()
      fit_foo = gamlss(ecdi~pbm(days), data = foo)
      foo$ecdi_eps = fit_foo$residuals
      dat = dat %>% dplyr::left_join(foo %>% dplyr::select(mrwid, ecdi_eps), by = "mrwid")
      
      sdO = dat %>% dplyr::filter(is.finite(ecdi)) %>% purrr::pluck("ecdi_eps") %>% sd()
      dat = dat %>% dplyr::mutate(ecdi_eps = ecdi_eps/sdO)
      ggplot(dat %>% dplyr::filter(eligible == 1), aes(x = days, y = ecdi_eps)) + geom_point() + stat_smooth()
      # REally not a way to definitively say
      
      
      # Look for not outliers 
      foo =  dat %>% dplyr::select(mrwid,nomlearn, days) %>% na.omit()
      fit_foo = gamlss(nomlearn~pbm(days), data = foo)
      foo$nomlearn_eps = fit_foo$residuals
      dat = dat %>% dplyr::left_join(foo %>% dplyr::select(mrwid, nomlearn_eps), by = "mrwid")
      
      foo =  dat %>% dplyr::select(mrwid,nomphys, days) %>% na.omit()
      fit_foo = gamlss(nomphys~pbm(days), data = foo)
      foo$nomphys_eps = fit_foo$residuals
      dat = dat %>% dplyr::left_join(foo %>% dplyr::select(mrwid, nomphys_eps), by = "mrwid")
      
      foo =  dat %>% dplyr::select(mrwid,nomsem, days) %>% na.omit()
      fit_foo = gamlss(nomsem~pb(days), data = foo)
      foo$nomsem_eps = fit_foo$residuals
      dat = dat %>% dplyr::left_join(foo %>% dplyr::select(mrwid, nomsem_eps), by = "mrwid")
      
      foo =  dat %>% dplyr::select(mrwid,nomsreg, days) %>% na.omit()
      fit_foo = gamlss(nomsreg~pb(days), data = foo)
      foo$nomsreg_eps = fit_foo$residuals
      dat = dat %>% dplyr::left_join(foo %>% dplyr::select(mrwid, nomsreg_eps), by = "mrwid")
      
      foo = dat %>% dplyr::filter(eligible==1)  %>% 
        dplyr::select(mrwid, days, nomlearn_eps:nomsreg_eps) %>% 
        pivot_longer(nomlearn_eps:nomsreg_eps, values_to = "score", names_to = "names") %>% 
        na.omit() %>% 
        dplyr::group_by(mrwid) %>% 
        dplyr::summarise(days = days[1], nom_eps = mean(score)) %>% 
        dplyr::mutate(nom_eps = nom_eps / sd(nom_eps))
      
      ggplot(foo , aes(x = days ,y = nom_eps)) + stat_smooth() + geom_point()
      ggplot(foo, aes(abs(nom_eps))) + geom_histogram()
      # Perhaps a few outliers with absolute residuals grader than  3 SD
      dat = dat %>% dplyr::left_join(foo, by = c("mrwid","days"))
      dat$eligible[abs(dat$nom_eps)>3.5] = 0
      mix= mix+1
      made_inelig[[mix]] = data.frame(N = sum(dat$eligible==0, na.rm = T), 
                                      reason = "NOM composite standardized score greater than 3.25 SD ", 
                                      N_2019 = sum(dat$eligible==0 & dat$mrwid>0 & dat$mrwid <990000, na.rm = T), 
                                      N_2022 = sum(dat$eligible==0 & dat$mrwid<0, na.rm = T), 
                                      N_2023 = sum(dat$eligible==0 & dat$mrwid>=990000, na.rm = T))
      dat = dat %>% dplyr::filter(eligible == 1)
      
  
      #### Get the item response data set ####
      kidsight_items = lex_xwalk$lex_kidsight24; kidsight_items = kidsight_items[!is.na(kidsight_items)] %>% sort()
      
      itemdat = dat
      names(itemdat) = plyr::mapvalues(names(itemdat), from = lex_xwalk$lex_ne22, to = lex_xwalk$lex_kidsight24) 
      itemdat = itemdat %>% 
        dplyr::select(mrwid,days,all_of(kidsight_items)) %>% 
        dplyr::mutate(years = days/365.25) %>% 
        dplyr::relocate(mrwid, years)
        
      

      # Obtain Cook's statistics for influence point based on age gradient.
      influence_df<-pblapply(1:length(kidsight_items), function(j){
        
        item_j = kidsight_items[j]
        tmp_j = itemdat %>% dplyr::select(mrwid,years,all_of(item_j)) %>% na.omit()
        names(tmp_j)[3] = "y"
        
        if(max(tmp_j$y, na.rm = T)==1){
          family_j = "binomial"
        } else {
          family_j = "poisson"
        }
        fit_j = glm(y~years, data = tmp_j , family = family_j)
        tmp_j$cook[!is.na(tmp_j$y)] = cooks.distance(fit_j)
        return(tmp_j %>% dplyr::select(mrwid,cook) %>% dplyr::mutate(item = item_j))
        
      }) %>% 
        dplyr::bind_rows() %>% 
        arrange(-cook) 
      
      
      influence_df$cook_cat = 3
      influence_df$cook_cat[influence_df$cook_cat<.5]=2
      influence_df$cook_cat[influence_df$cook<.3] = 1
      influence_df$cook_cat[influence_df$cook<.1] = 0
      influence_df$cook_cat = as.ordered(influence_df$cook_cat)
      
      
      # Obtain intercept and trajectory information for binary items
      # pdf(file = paste0(path,"/calibration-kidsights/Jun_2024/age_gradients_binary.pdf"), height = 5, width = 5)
      # age_gradients_binary = pblapply(1:length(kidsight_items), function(j){
      #   item_j = kidsight_items[j]
      #   tmp_j = itemdat %>% dplyr::select(mrwid,years,all_of(item_j)) %>% na.omit()
      #   names(tmp_j)[3] = "y"
      #   tmp_j$item = item_j
      # 
      #   stem_j = lex_xwalk$label[which(lex_xwalk$lex_kidsight24==item_j)]
      #   domain_j =  lex_xwalk$domain3[which(lex_xwalk$lex_kidsight==item_j)]
      # 
      #   tmp_j = tmp_j %>% left_join(influence_df, by = c("item", "mrwid"))
      # 
      #   if(max(tmp_j$y)==1){
      # 
      #     plot_j = ggplot() +
      #       geom_point(data = tmp_j, aes(x = years, y = y), alpha = 0.3, shape = 124) +
      #       geom_text(data = tmp_j %>% dplyr::filter(cook_cat!=0), aes(x = years, y = y, label = as.character(cook_cat)), col = "red") +
      #       stat_smooth(data = tmp_j, aes(x = years, y = y), se = F, method = "glm", method.args = list(family = "binomial"), col = "blue", alpha = 0.5) +
      #       stat_smooth(data = tmp_j %>% dplyr::filter(cook_cat!="3"), aes(x = years, y = y), se = F, method = "glm", method.args = list(family = "binomial"), col = "purple", shape = 2) +
      #       stat_smooth(data = tmp_j %>% dplyr::filter(cook_cat!="3" & cook_cat!="2"), aes(x = years, y = y), se = F, method = "glm", method.args = list(family = "binomial"), col = "green", shape = 3) +
      #       ggtitle(label = paste0(item_j, "(",domain_j,")"), subtitle = stem_j) +
      #       coord_cartesian(xlim = c(0,6))
      #     print(plot_j)
      #     fit_j = glm(y~years, data = tmp_j, family = "binomial")
      #     coefs_j = coef(fit_j)
      #     return(data.frame(item = item_j, b0 = coefs_j[1], b1 = coefs_j[2]))
      #   } else {
      #     return(NULL)
      #   }
      # }) %>% dplyr::bind_rows()
      # dev.off()
      # readr::write_rds(age_gradients_binary, file = paste0(intermediate_path,"/Jun24/age_gradients_binary.rds"))
      age_gradients_binary = readr::read_rds(file =  paste0(intermediate_path,"/Jun24/age_gradients_binary.rds"))
      
      
      # Fromp plots clear to remove item responses with Cooks > .3 
      for(i in 1:sum(influence_df$cook>.3)){
        idx_i = which(itemdat$mrwid == influence_df$mrwid[i])
        item_i = influence_df$item[i]
        itemdat[idx_i, item_i] = NA
      }
      
      
      
      # Remove any item with (a) over an 90% endorsement rate beginning at birth or (b)  age gradient <=0
      age_gradients_binary = age_gradients_binary %>% 
        dplyr::mutate(pr0=plogis(b0), 
                      remove_b0 = ifelse(pr0>.90, 1, 0), 
                      remove_b1 = ifelse(b1<= 0, 1, 0)) %>% 
        arrange(-remove_b0)
      items_discard = age_gradients_binary %>% dplyr::filter(remove_b0==1 | remove_b1 == 1) %>% purrr::pluck("item")
      
      
      rix = rix+1
      removed_items[[rix]] = age_gradients_binary %>% dplyr::filter(remove_b0==1 | remove_b1 == 1)
      
      
      kidsight_items = setdiff(kidsight_items, items_discard)
      itemdat = itemdat %>% dplyr::select(-any_of(items_discard))
      
      
      # Remove any items with no variability
      sd_items = itemdat %>% 
        dplyr::select(mrwid, any_of(kidsight_items)) %>% 
        pivot_longer(col = any_of(kidsight_items), names_to = "item", values_to = "y") %>% 
        na.omit() %>% 
        dplyr::group_by(item) %>% 
        dplyr::summarise(sd_y = sd(y)) %>% 
        arrange(sd_y) %>% 
        dplyr::filter(sd_y<.01)
      nrow(sd_items)#
      
      items_discard = sd_items %>% purrr::pluck("item")
      rix = rix+1
      removed_items[[rix]] = sd_items
      
      itemdat = itemdat %>% dplyr::select(-all_of(items_discard))
      kidsight_items = setdiff(kidsight_items, items_discard)
      
      
      
      # Remove all response observations with only NA
      itemdat = itemdat %>% 
        dplyr::mutate(J_ans = itemdat %>% dplyr::select(all_of(kidsight_items)) %>% apply(1,function(x){sum(!is.na(x))}), 
                      eligible = ifelse(J_ans==0, 0, 1)) %>% 
        dplyr::relocate(eligible) %>% 
        dplyr::select(-J_ans)
      
      mix = mix+1
      made_inelig[[mix]] = data.frame(N = sum(itemdat$eligible==0), N_2022 = sum(itemdat$eligible==0 & itemdat$mrwid<0),reason = "All missing responses" )
      
      itemdat = itemdat %>% dplyr::filter(eligible == 1)
      
       
      # # ##### Round 1 IRT Fitting #####
      # setwd(paste0(intermediate_path, "/Jun24"))
      # library(MplusAutomation)
      # prepareMplusData(itemdat, filename = "itemdat_v1.dat", inpfile = T)
      # pars_v1 = 
      # fit_mirt_v1<-
      #   mirt::mirt(
      #             data = itemdat %>% dplyr::select(any_of(kidsight_items)),
      #             model = 1,
      #             covdata = itemdat %>% dplyr::select(years),
      #             formula = ~ log(years+.10),
      #             quadpts = 61*2,
      #             technical = list(theta_lim = c(-20,15),
      #                              NCYCLES = 2000), 
      #             TOL = 1E-5, 
      #             optimizer = "NR", 
      #             pars = mplus2pars(itemdat = itemdat, item_names = kidsight_items, fit_mplus = MplusAutomation::readModels(target = "fit_mirt_v1.out"))
      #   )
      #readr::write_rds(fit_mirt_v1, file = paste0(intermediate_path,"/Jun24/fit_mirt1.rds"))
      #readr::write_rds(itemdat, file = paste0(intermediate_path,"/Jun24/itemdat1.rds"))
      fit_mirt=readr::read_rds(file = paste0(intermediate_path,"/Jun24/fit_mirt1.rds"))
      itemdat = readr::read_rds(file = paste0(intermediate_path, "/Jun24/itemdat1.rds"))
      fscores = data.frame(
        years = itemdat$years,
        eap = fit_mirt %>% mirt::fscores(method = "EAP", theta_lim = c(-20,15), quadpts = 61*4, full.scores.SE = F)
      )
      
      
      values = mod2values(fit_mirt)
      difficulties_df = values %>% dplyr::filter(startsWith(name,"a") | startsWith(name,"d")) %>%
        dplyr::select(item,class,name,value) %>%
        pivot_wider(id_cols = c("item","class")) %>%
        dplyr::mutate(d1 = ifelse(class == 'dich',d,d1)) %>%
        dplyr::select(-d) %>%
        dplyr::mutate(d1 = -d1/a1,
                      d2 = -d2/a1,
                      d3 = -d3/a1,
                      d4 = -d4/a1,
                      d5 = -d5/a1) %>%
        dplyr::select(item,d1:d5) %>%
        pivot_longer(cols = d1:d5) %>%
        na.omit() %>%
        arrange(value)
      
      ggplot() +
        geom_histogram(data = fscores, aes(F1,y = ..density..), alpha = 0.7) +
        geom_text(data = difficulties_df, aes(x=value,y=-0.01, label = item ), angle = 90, col = "blue") +
        geom_point(data = difficulties_df, aes(x=value,y=-0.005, label = item ), shape = 124, col = "blue")
      # Based on this, remove CC7, an as they contribute no inormation
      
      
      rix = rix+1
      removed_items[[rix]] = data.frame(difficulties_df %>% dplyr::filter(item %in% c("CC7", "AA3"))) %>% dplyr::mutate(Reason = "Item location outside of range of person location.")
      
      # # Export a table explaining which items were removed
      # removed_items_df = removed_items[[1]] %>% 
      #   dplyr::mutate(Reason = ifelse(remove_b0==1,"Predicted item endorsement > .90 at 0 months.", "Non-positive association with age.")) %>% 
      #   dplyr::select(item, Reason) %>% 
      #   dplyr::bind_rows(
      #     removed_items[[2]] %>% 
      #       dplyr::mutate(Reason = "No item variability.") %>% 
      #       dplyr::select(item,Reason)
      #   ) %>% 
      #   dplyr::bind_rows(
      #     removed_items[[3]] %>% 
      #       dplyr::select(item,Reason)
      #   ) %>% 
      #   dplyr::left_join(
      #     lex_xwalk %>% 
      #       dplyr::mutate(item = lex_kidsight) %>% 
      #       dplyr::select(item,label,domain3), 
      #     by = "item"
      #   ) %>% 
      #   dplyr::relocate(item,label,domain3,Reason)
      # write.csv(removed_items_df, file = paste0(tables_figures_path,"/removed_items.csv"))
      
      items_discard = removed_items[[rix]]$item; 
      kidsight_items = setdiff(kidsight_items, items_discard)
      values = values %>% dplyr::filter(!(item %in% items_discard))
      values$parnum = 1:nrow(values)
      

      # ### Round 2 IRT Fitting: Removed the two items ####
      itemdat = itemdat %>% dplyr::select(-dplyr::all_of(items_discard))
      # fit_mirt <-
      #   mirt::mirt(
      #     data = itemdat %>% dplyr::select(any_of(kidsight_items)),
      #     model = 1,
      #     covdata = itemdat %>% dplyr::select(years),
      #     formula = ~ log(years+.10),
      #     quadpts = 61*2,
      #     technical = list(theta_lim = c(-20,15),
      #                      NCYCLES = 2000),
      #     optimizer = "NR", #"nloptr",
      #     #nloptr_args = list(opts = list("algorithm" = "NLOPT_LD_TNEWTON_PRECOND_RESTART")),
      #     pars = values,
      #     TOL = 1E-5
      #   )#END MIRT
      # saveRDS(fit_mirt, file = paste0(intermediate_path, "/Jun24/fit_mirt2.rds"))
      # saveRDS(itemdat, file = paste0(intermediate_path, "/Jun24/itemdat2.rds"))
      fit_mirt = readr::read_rds(file = paste0(intermediate_path, "/Jun24/fit_mirt2.rds"))
      itemdat = readr::read_rds(file = paste0(intermediate_path,"/Jun24/itemdat2.rds"))
      
      
      values = mod2values(fit_mirt)
      difficulties_df = values %>% dplyr::filter(startsWith(name,"a") | startsWith(name,"d")) %>%
        dplyr::select(item,class,name,value) %>%
        pivot_wider(id_cols = c("item","class")) %>%
        dplyr::mutate(d1 = ifelse(class == 'dich',d,d1)) %>%
        dplyr::select(-d) %>%
        dplyr::mutate(d1 = -d1/a1,
                      d2 = -d2/a1,
                      d3 = -d3/a1,
                      d4 = -d4/a1,
                      d5 = -d5/a1) %>%
        dplyr::select(item,d1:d5) %>%
        pivot_longer(cols = d1:d5) %>%
        na.omit() %>%
        arrange(value)
      fscores = data.frame(
        mrwid = itemdat$mrwid,
        years = itemdat$years,
        eap = fit_mirt %>% mirt::fscores(method = "EAP", theta_lim = c(-20,15), full.scores.SE = F)
      )
      
      plot1 = ggplot() +
        geom_histogram(data = fscores, aes(F1,y = ..density..), alpha = 0.7) +
        geom_text(data = difficulties_df, aes(x=value,y=-0.01, label = item ), angle = 90, col = "blue") +
        geom_point(data = difficulties_df, aes(x=value,y=-0.005), shape = 124, col = "blue")
      print(plot1)
      # Looks good
      
      ggplot(fscores, aes(x = years, y = F1)) + geom_point(alpha = 0.5) + stat_smooth(method = lm, formula = y~log(x+.1))
      # Need to remove one outlier at about 2 months
      
      fscores = fscores %>% dplyr::mutate(eps = loess(F1~years, data = fscores)$residuals)
      id_max_eps = fscores$mrwid[which.max(fscores$eps)]
      itemdat$eligible[itemdat$mrwid==id_max_eps]=0
      mix = mix+1
      made_inelig[[mix]] = data.frame(N = sum(itemdat$eligible==0), N_2022 = sum(itemdat$eligible==0 & itemdat$mrwid<0), reason = "Outlier after Round 1 IRT fitting.")
      itemdat = itemdat %>% dplyr::filter(eligible==1)
      
      
      
      # ### Round 3 IRT Fitting: Removed the 2 month old outlier and check cook's distance ####
      library(MplusAutomation)
      #prepareMplusData(itemdat, filename = "itemdat_v3.dat", inpfile = T)
      fit_mplus = readModels(target = file.path(intermediate_path,"/Jun24/fit_mirt_v3.out"))
      cooks_mplus = fit_mplus$savedata %>% dplyr::select(MRWID, OUTCOOK) %>% dplyr::rename_all(tolower) %>%
        dplyr::left_join(itemdat %>% dplyr::select(mrwid,years)) %>%
        dplyr::mutate(study = "NE22",
                      study = ifelse(mrwid>0 & mrwid<990000, "NE20", study),
                      study = ifelse(mrwid>=990000, "RAPID22", study)
                      )
      ggplot(cooks_mplus, aes(x=years, y = outcook, col = study, fill = study)) + geom_point()
      sum(cooks_mplus$outcook>1.25)

      #Let's get rid of anything with cooks distance greater than 1.25
      itemdat = itemdat %>% dplyr::left_join(cooks_mplus %>% dplyr::select(mrwid,outcook), by = "mrwid")
      itemdat$eligible[itemdat$outcook>1.25] = 0
      mix = mix+1
      
      foo = cooks_mplus %>% dplyr::group_by(study) %>% dplyr::summarise(N = sum(outcook>1.25))
      
      made_inelig[[mix]] = data.frame(reason = "Cook's distance following IRT model fit greater than 1.25", N_2019 = foo$N[1], N_2022 = foo$N[2], N_2023 = foo$N[3])
      itemdat = itemdat %>% dplyr::filter(eligible==1)
      itemdat = itemdat %>% dplyr::select(-outcook)

      


    
      
      #Let's refit 
      # fit_mirt <-
      #   mirt::mirt(
      #     data = itemdat %>% dplyr::select(any_of(kidsight_items)),
      #     model = 1,
      #     covdata = itemdat %>% dplyr::select(years),
      #     formula = ~ log(years+.10),
      #     quadpts = 61*2,
      #     technical = list(theta_lim = c(-20,15),
      #                      NCYCLES = 2000),
      #     optimizer = "NR", #"nloptr",
      #     #nloptr_args = list(opts = list("algorithm" = "NLOPT_LD_TNEWTON_PRECOND_RESTART")),
      #     pars = mod2values(fit_mirt),
      #     TOL = 1E-5
      #   )#END MIRT
      #  readr::write_rds(itemdat, file = paste0(intermediate_path,"/Jun24/itemdat3.rds"))
      #  readr::write_rds(fit_mirt, file = paste0(intermediate_path,"/Jun24/fit_mirt3.rds"))
        fit_mirt = readr::read_rds(file = paste0(intermediate_path,"/Jun24/fit_mirt3.rds"))
        itemdat = readr::read_rds(file = paste0(intermediate_path,"/Jun24/itemdat3.rds"))
      
      
        
        
        values = mod2values(fit_mirt)
        difficulties_df = values %>% dplyr::filter(startsWith(name,"a") | startsWith(name,"d")) %>%
          dplyr::select(item,class,name,value) %>%
          pivot_wider(id_cols = c("item","class")) %>%
          dplyr::mutate(d1 = ifelse(class == 'dich',d,d1)) %>%
          dplyr::select(-d) %>%
          dplyr::mutate(d1 = -d1/a1,
                        d2 = -d2/a1,
                        d3 = -d3/a1,
                        d4 = -d4/a1,
                        d5 = -d5/a1) %>%
          dplyr::select(item,d1:d5) %>%
          pivot_longer(cols = d1:d5) %>%
          na.omit() %>%
          arrange(value)
        fscores = data.frame(
          mrwid = itemdat$mrwid,
          years = itemdat$years,
          eap = fit_mirt %>% mirt::fscores(method = "EAP", theta_lim = c(-20,15), full.scores.SE = F)
        )  
      itemfit_df = itemfit(fit_mirt, fit_stats = "X2") %>%
        dplyr::left_join(
      
          difficulties_df %>% dplyr::filter(name == "d1"), by = "item"
      
        ) %>%
        dplyr::left_join(
      
          values %>%
            dplyr::filter(name == "a1") %>%
            dplyr::select(item,value) %>%
            dplyr::rename_at("value",function(x)"a1"),
          by = "item"
      
      
        ) %>%
        dplyr::mutate(rmsea = ifelse(RMSEA.X2>.08,.08,RMSEA.X2),
                      okay = rmsea<.08) %>%
        arrange(-RMSEA.X2)
      
      # Get rid of the missfitting items greater than: 
      #   1. Greater than .08 and d1 values greater than -5 
      #  -or- 
      #   2. RMSEA greater than .16 (any difficulty value)
      # The above done because we need some items for younger kids because of low reliability there.
      rix = rix+1 
      removed_items[[rix]] = itemfit_df %>% dplyr::filter( (value > -5 & RMSEA.X2 >.08) | RMSEA.X2 >.16)
      items_discard =  removed_items[[rix]] %>% purrr::pluck("item")
      kidsight_items = setdiff(kidsight_items,items_discard)
      
      
      #### Round 4: Get rid of item with horrible rmsea and refit ####
      # itemdat = itemdat %>% dplyr::select(-dplyr::all_of(items_discard))
      # values = values %>% dplyr::filter(!(item %in% items_discard))
      # values$parnum = 1:nrow(values)
      #  fit_mirt <-
      #    mirt::mirt(
      #      data = itemdat %>% dplyr::select(any_of(kidsight_items)),
      #      model = 1,
      #      covdata = itemdat %>% dplyr::select(years),
      #      formula = ~ log(years+.10),
      #      quadpts = 61*2,
      #      technical = list(theta_lim = c(-20,15),
      #                       NCYCLES = 2000),
      #      optimizer = "NR", #"nloptr",
      #      #nloptr_args = list(opts = list("algorithm" = "NLOPT_LD_TNEWTON_PRECOND_RESTART")),
      #      pars = values,
      #      TOL = 1E-5
      #    )#END MIRT
      #    readr::write_rds(itemdat, file = paste0(intermediate_path,"/Jun24/itemdat4.rds"))
      #    readr::write_rds(fit_mirt, file = paste0(intermediate_path,"/Jun24/fit_mirt4.rds"))
       fit_mirt = readr::read_rds(file = paste0(intermediate_path,"/Jun24/fit_mirt4.rds"))
       itemdat = readr::read_rds(file = paste0(intermediate_path,"/Jun24/itemdat4.rds"))
      
       
       values = mod2values(fit_mirt)
       difficulties_df = values %>% dplyr::filter(startsWith(name,"a") | startsWith(name,"d")) %>%
         dplyr::select(item,class,name,value) %>%
         pivot_wider(id_cols = c("item","class")) %>%
         dplyr::mutate(d1 = ifelse(class == 'dich',d,d1)) %>%
         dplyr::select(-d) %>%
         dplyr::mutate(d1 = -d1/a1,
                       d2 = -d2/a1,
                       d3 = -d3/a1,
                       d4 = -d4/a1,
                       d5 = -d5/a1) %>%
         dplyr::select(item,d1:d5) %>%
         pivot_longer(cols = d1:d5) %>%
         na.omit() %>%
         arrange(value)
       fscores = data.frame(
         mrwid = itemdat$mrwid,
         years = itemdat$years,
         eap = fit_mirt %>% mirt::fscores(method = "EAP", theta_lim = c(-20,15), full.scores.SE = F)
       )
       
       plot1 = ggplot() +
         geom_histogram(data = fscores, aes(F1,y = ..density..), alpha = 0.7) +
         geom_text(data = difficulties_df, aes(x=value,y=-0.01, label = item ), angle = 90, col = "blue") +
         geom_point(data = difficulties_df, aes(x=value,y=-0.005), shape = 124, col = "blue")
       print(plot1)
       # Looks good
       
       ggplot(fscores, aes(x = years, y = F1)) + geom_point(alpha = 0.5) + stat_smooth(method = lm, formula = y~log(x+.1))

       
       
  
       itemfit_df = itemfit(fit_mirt, fit_stats = "X2") %>%
         dplyr::left_join(
           
           difficulties_df %>% dplyr::filter(name == "d1"), by = "item"
           
         ) %>%
         dplyr::left_join(
           
           values %>%
             dplyr::filter(name == "a1") %>%
             dplyr::select(item,value) %>%
             dplyr::rename_at("value",function(x)"a1"),
           by = "item"
           
           
         ) %>%
         dplyr::mutate(rmsea = ifelse(RMSEA.X2>.08,.08,RMSEA.X2),
                       okay = rmsea<.08) %>%
         arrange(-RMSEA.X2) # looks good
       
      # Start a reportable parameter estimates table
      values = mod2values(fit_mirt)

      ds_df = values %>% dplyr::filter(startsWith(name,"a") | startsWith(name,"d")) %>%
        dplyr::select(item,class,name,value) %>%
        pivot_wider(id_cols = c("item","class")) %>%
        dplyr::mutate(d1 = ifelse(class == 'dich',d,d1)) %>%
        dplyr::select(-d) %>%
        dplyr::mutate(d1 = -d1/a1,
                      d2 = -d2/a1,
                      d3 = -d3/a1,
                      d4 = -d4/a1,
                      d5 = -d5/a1) %>%
        dplyr::select(item,d1:d5) %>%
        dplyr::mutate(across(d1:d5,function(x){round(x,3)}))

      as_df = values %>% dplyr::filter(name=="a1") %>% dplyr::select(item,value) %>%
        dplyr::mutate(value = round(value,3))
      names(as_df) = c("item","a1")

       # partable_df = data.frame(
       #  item = kidsight_items) %>% 
       #  dplyr::left_join(lex_xwalk %>%
       #                     dplyr::mutate(item = lex_kidsight24) %>%
       #                     dplyr::select(item,label,domain3), by = "item"
       #                   )  %>%
       #  dplyr::left_join(itemfit(fit_mirt, fit_stats = "X2") %>%
       #                     dplyr::select(item,starts_with("RMSEA")) %>%
       #                     dplyr::mutate(across(where(is.numeric),function(x)round(x,3))),
       #                   by = "item"
       #                   ) %>%
       #  dplyr::left_join(as_df,by="item") %>%
       #  dplyr::left_join(ds_df, by = "item") %>%
       #  dplyr::arrange(d1)
       # 
       # # Get invariant items
       # get_alignment<-function(file_in, name_out){
       #   hi = MIE::extractAlignment(file = file_in)
       # 
       #   bye = hi$non.invariant.pars %>%
       #     data.frame() %>%
       #     dplyr::mutate(par = row.names(hi$non.invariant.pars)) %>%
       #     tidyr::unite("flag",starts_with("X"), na.rm = TRUE, remove = FALSE, sep = "") %>%
       #     dplyr::mutate(flag = ifelse(startsWith(flag,"X"),"X",flag),
       #                   item = stringr::str_remove_all(par, pattern = "Threshold "),
       #                   item = stringr::str_remove_all(item, pattern = "Loadings  F by "),
       #                   item = stringr::str_split_i(item,"\\$",1)
       #     ) %>%
       #     dplyr::filter(flag == "X") %>%
       #     dplyr::group_by(item) %>%
       #     dplyr::summarise(flag = flag[1])
       # 
       #   names(bye)[2] = name_out
       # 
       #   return(bye)
       # }
       #  # Education
       #  edu_dif = get_alignment(file_in="criterion_invariance/edu4_alignment.out",name_out="edu_dif")
       #  partable_df = partable_df %>% dplyr::left_join(edu_dif, by = "item")
       #  # Race
       #  race_dif = get_alignment(file_in="criterion_invariance/race_alignment.out",name_out="race_dif")
       #  partable_df = partable_df %>% dplyr::left_join(race_dif, by = "item")
       #  # Geography
       #  ne_dif = get_alignment(file_in="criterion_invariance/ne_alignment.out",name_out="ne_dif")
       #  partable_df = partable_df %>% dplyr::left_join(ne_dif, by = "item")
      #write.csv(partable_df, file = paste0(tables_figures_path,"/partable_df.csv"))
      
      # Get removed items table
      removed_df = removed_items[[1]] %>% dplyr::mutate(Reason =ifelse(pr0>.90,"Predicted item endorsement > .90 at 0 months.", "Non-positive association with age.")) %>% dplyr::select(item,Reason) %>%
        dplyr::bind_rows(removed_items[[3]] %>% dplyr::select(item,Reason)) %>%
        dplyr::bind_rows(removed_items[[4]] %>% dplyr::mutate(Reason = paste0("Poor item fit (RMSEA = ", round(RMSEA.X2,2),").")) %>% dplyr::select(item,Reason) ) %>%
        dplyr::left_join(lex_xwalk %>% dplyr::select(lex_kidsight24,domain3,label) %>% dplyr::rename(item=lex_kidsight24), by = "item") %>%
        dplyr::mutate(j = 1:n()) %>%
        dplyr::select(item,j,label,domain3,Reason)

      
      
### Save out a list with important data files 
      out_list = list(dat = dat, raw_NE22 = raw22, removed = removed_df, made_inelig = made_inelig)
      setwd(onedrive_phase2_data_path)
      setwd("..")
      setwd("Publications/Kidsights-Home-Learn-Env-2020-23/Data/intermediate/")
      readr::write_rds(out_list, file = "0-data_list_scored.rds", compress ="gz")

