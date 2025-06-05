    rm(list = ls())


    library(tidyverse)
    library(readr)
    library(mice)
    library(doSNOW)
    library(doRNG)
    library(brms)
    library(haven)
    library(tidybayes)
    library(srvyr)
    
    origwd = getwd()

    # Set working directories
    root = "C:/Users/waldmanm"
    #"C:/Users/marcu"
    #"D:/"
    path =  paste0(root,"/Dropbox/UNMC/phase-2")
    intermediate_path = paste0(path,"/data-files/intermediate-files")
    github_path = paste0(root,"/git-repositories/Kidsights-Home-Learn-Env-2020-23/Code/0-Kidsights-HLE-2020-23-ETL")
    onedrive_path = file.path(root,"University of Nebraska Medical Center", "Kidsights Data - Documents", "General")
    
    setwd(github_path)
    
    # Source in functions and subroutines 
    subroutines = list.files(paste0(github_path,"/subroutines"), pattern = ".R", full.names = T)
    for(x in 1:length(subroutines)){source(file = subroutines[x])}
    source("utils/utils.R")
    
    
    # Load in the scored and raw datasets
    scored_list = readr::read_rds(file = file.path(onedrive_path, "Publications","Kidsights-Home-Learn-Env-2020-23", "Data", "intermediate", "0-data_list_scored.rds"))
    
    
    
    
    # Load in the Kidsight22 scores and weights
    kidsight22 = readr::read_rds(file = paste0(path, "/data-files/analytic-files/analytic-wgts-scores-unmc22-02mar2023.rds"))
    
    
    #Fix the old and new weight
    new_wgts =  readr::read_rds(file = paste0(path, "/data-files/intermediate-files/unmc22_survey_weights_14oct2023.rds"))
    names(new_wgts)[2] = c("wgt2")
    kidsight22 = kidsight22 %>% dplyr::left_join(new_wgts, by = "ResponseId")
    
    # Load in the raw survey responses
    elig_list = readr::read_rds(file = paste0(intermediate_path, "/elig_list.rds"))
    
    # Load in the made_inelig
    made_inelig = readr::read_rds(file = paste0(intermediate_path, "/Jun24/made_inelig.rds"))
    
    # Specify itermediate variables
    intermed_vars = c(
      paste0("PACE",1:10,"r"), # Parent ACEs
      "acedivorce_rc", "acedeath_rc", "acejail_rc", "acedv_rc", "aceviolence_rc", "acementalill_rc", "aceaddict_rc", "aceracism_rc", #Child aces
      paste0("FCI.D.",paste0(1:11)),  
      paste0("FCI.C.",paste0(1:11)), 
      "CQR004", "a1_parent", "a1_female", #Education (Mom and dad)
      "CQFA005", #Difficulty covering basics, 
      "CQFB003", #Handling handles_demands
      "CQFA006", #Food insecurity, 
      "inc99", "FQLIVE1_1", "FQLIVE1_2", # To help derive % above/below poverty line
      "CQFA010", #Someone to turn to for emot. support
      "CQFB013", "CQFB014", "CQFB015", "CQFB016", #Anxiety and depression variables, 
      "CQFB007", # Childcare, 
      paste0("CQR007_",1:8)
    )
    
    # Specify analytic variables
    derived_vars = 
      c(
        "childACES",# Treat as poisson
        "fciactivities_rev", # Treat as poisson
        "fcimaterials_rev", # Treat as posson
        "depanx_mom", # Treat as poisson
        "depanx_pop",
        "ACES_mom",
        "ACES_pop",
        "educ_mom",
        "educ_pop", 
        "snap","wic"
      )
    
    # Specify imputation vars
    impute_vars = c(
      "troublebasics",
      "foodinsuff",  
      "handles_demands",
      "a1_educG",
      "emotsupport",
      "race","raceG", 
      "hisp", 
      "childcare", 
      "years",
      "female",
      "metro",
      intermed_vars
    ) 
    
    factor_vars = c("a1_educ","race","foodinsuff")
    
    analytic_vars = c(
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
      "metro",
      # Responder type
      "a1_female",
      # Others
      "a1_educG", "a1_depanx", "a1_ACES", "childcare", 
      # Services
      "snap","wic"
    )
    
    
    #### Create an analytic data set  ###
    
    # See C:\Users\marcu\Dropbox\UNMC\phase-2\calibration-kidsights\Jun_2024\0-scratchpaper-Jun_2024-calibration-kidsights.R 
    eligible_Rids = readr::read_rds(file = paste0(intermediate_path, "/Jun24/NE22_eligibles.rds"))
    
    # Exclusion restrictions table
    exclusion_list = list(NULL)
    exclusion_list[[1]] = data.frame( n = made_inelig[[1]]$N, Reason = "Identified as fraudulent response, did not provide informed consent, or not did not reside in Nebraska." )
    exclusion_list[[2]] = data.frame( n = made_inelig[[2]]$N, Reason = "Non-English survey administration." )
    exclusion_list[[3]] = data.frame( n = made_inelig[[4]]$N_2022, Reason = "No information on child's age." )
    exclusion_list[[4]] = data.frame( n = made_inelig[[5]]$N_2022 + made_inelig[[6]]$N_2022, Reason = "Overall CREDI scores greater than 5SD or less than 5SD below sample average." )
    exclusion_list[[5]] = data.frame( n = made_inelig[[9]]$N_2022 + made_inelig[[10]]$N_2022, Reason = "Influence/outliers: Multivariate cook's distance following IRT calibration of Kidsights greater than 1.25")
    exclusion_table = exclusion_list %>% dplyr::bind_rows()
    
    
    # Apply exclusion restrictions
    dat22 = elig_list$survey %>% 
      dplyr::select(-any_of( setdiff(names(kidsight22),"ResponseId") )) %>% 
      dplyr::left_join(kidsight22, by = "ResponseId") %>% 
      dplyr::filter(ResponseId %in% eligible_Rids$ResponseId)
    
    flow_chart = exclusion_table %>% dplyr::mutate(rid=1:n()) %>% dplyr::arrange(-rid) %>% dplyr::mutate(cumsum = cumsum(n) + nrow(dat22)) %>% 
      dplyr::relocate(rid,Reason)
    
    
    # Get metropolitan information
    ziplist = readr::read_rds(file = file.path(path, "KidsightsDashboard/DashboardNE22/KidsightDashboardNE22/data/ziplist_smoothed.rds"))
    
    
    # Transform the variables
    dat22 = dat22 %>%    
      dplyr::left_join(ziplist$db %>% dplyr::select(zip,lat,lng,metro,smedhhinc,slpopdense), by = "zip") %>% 
      recode_FCI() %>% 
      recode_CQR007() %>% 
      get_PACEr()  %>% 
      get_race() %>% 
      get_race_Ghandour() %>% 
      get_educ() %>% 
      get_educ_Ghandour() %>% 
      get_misc() %>% 
      dplyr::mutate(across(any_of(factor_vars), char2factor))
    
    
  
    #   ###### Conduct multiple imputation ######

     
     agreed_obs = dat22 %>% 
       get_derived_vars(path = path) %>% 
       ETL_agreed_vars() %>% 
       dplyr::left_join(psy_scores(path = path) %>% dplyr::select(-mrwid), by = "ResponseId")
    readr::write_rds(agreed_obs, file =file.path(onedrive_path,"Publications","Kidsights-Home-Learn-Env-2020-23","/Data/analytic/agreed_obs.rds"))
    haven::write_sav(agreed_obs, path = file.path(onedrive_path,"Publications","Kidsights-Home-Learn-Env-2020-23","/Data/analytic/agreed_obs.sav"))
    haven::write_dta(agreed_obs, path = file.path(onedrive_path,"Publications","Kidsights-Home-Learn-Env-2020-23","/Data/analytic/agreed_obs.dta"))
    
    writexl::write_xlsx(data.frame(var = names(agreed_obs), definition = c(
      "Unique individual identifier", 
      "Kidsight scaled score (unstandardized)", 
      "dscore (unstandardized)", 
      "CREDI cognition score (undstandardized)", 
      "CREDI langauge score (unstandandardized)", 
      "CREDI motor score (unstandardized)", 
      "CREDI socioemotional development score (unstandardized)", 
      "CREDI overall score (unstandardized)", 
      "ECDI score (unstandardized)", 
      "HRTL 2019 early learning factor scores", 
      "HRTL 2019 physical development factor scores", 
      "HRTL 2019 socioemtional learning factor scores", 
      "HRTL 2019 selfregulation factor scores", 
      "Food insufficiency score (3 category, collapsed)",
      "Caregiver reports trouble meeting basics", 
      "Caregiver reports that have emotional support system", 
      "Child attends 10+ hours of childcare (any type)", 
      "Maternal ACEs count", 
      "Paternal ACEs count", 
      "Maternal anxiety and depressive symptoms: PHQ + GAD-2 total scores", 
      "Paternal anxiety and depressive symtoms: PHQ + GAD-2 total scores", 
      "Caregiver reports they can handle demands of parenting well", 
      "Home learning environment: Activity subscale total score", 
      "Home learning environment: Materials subscale total score", 
      "Child ACEs count", 
      "Child's age in years",
      "Child identified as female", 
      "Child's race", 
      "Maternal education", 
      "Paternal education", 
      "Residential zipcode", 
      "Residential zipcode latitude",
      "Residental zipcode longitude",
      "Residential address in metropolitan area", 
      "Zipcode median househould income (standardized)",
      "Zipcode population density (standardized)", 
      "Responding adult was female", 
      "Family size", 
      "Responding adult's education level (Ghandour et al 2024 classifications)", 
      "Responding adult's depression and anxiety symptoms: PHQ-2 + GAD-2 total score",
      "Responding adult's ACEs count",
      "REDUNDANT. 10+ of childcare", 
      "Family enrolled in SNAP", 
      "Family enrolled in WIC", 
      "Federal poverty line category", 
      "Child' race (Ghandour et al 2024 designations)", 
      "Child's GSED Psychosocial scores"
    )), 
    path = file.path(onedrive_path,"Publications","Kidsights-Home-Learn-Env-2020-23","/Data/analytic/DICTIONARY - agree_obs.xlsx"))
    
    
    
    # toimpute = dat22 %>%
    #   dplyr::select(all_of(impute_vars)) %>%
    #   get_derived_vars(path = path) %>%
    #   dplyr::mutate(across(everything(), zap_attributes)) %>%
    #   dplyr::select(any_of(c(impute_vars,analytic_vars)))
    # 
    # # Below sanity checks all look good
    # # agreed_obs %>% dplyr::select(foodinsuff, troublebasics, snap, wic, fpl_cat) %>% cor(use = "complete.obs")
    # # agreed_obs %>% dplyr::group_by(a1_educ) %>% dplyr::summarise(fpl = mean(fpl_cat, na.rm = T))
    # # agreed_obs %>% dplyr::group_by(raceG) %>% dplyr::summarise(fpl = mean(fpl_cat, na.rm = T))
    # 
    # 
    #  cl <-parallel::makeCluster(16); registerDoSNOW(cl)
    #    implist <- impute(toimpute, path = path, dat22 = dat22)
    #    readr::write_rds(implist, file = "data/implist.rds")
    #  parallel::stopCluster(cl)
    # 
    # #  Add psychosocial socres to implist
    # psy_df = psy_scores() %>% dplyr::select(-mrwid)
    # implist = readr::read_rds(file = "data/implist.rds") %>% bind2implist(df_in = psy_df)
    # readr::write_rds(implist, file = "data/implist.rds")
    implist = readr::read_rds(file = "data/implist.rds") 

    # # Add in CREDI, ECDI, and NOM factor scores
    # implist <- pbapply::pblapply(1:length(implist), function(m){ 
    #   implist[[m]] %>%  
    #     dplyr::select(-starts_with("dscore"), -starts_with("credi"), -starts_with("ecdi"), -starts_with("nom")) %>% 
    #     dplyr::left_join(
    #       dat22 %>% dplyr::select(ResponseId, dscore, credi_COG:credi_OVERALL, ecdi, nomlearn:nomsreg), 
    #       by = "ResponseId")
    # })
    
    # agreed_implist = pbapply::pblapply(1:length(implist), function(m){
    #   return(implist[[1]] %>% get_derived_vars() %>% ETL_agreed_vars())
    # })
    
    
    
    # Make design weights
    #wgts_list <- make_design_weights(implist)
    wgts_list = readr::read_rds(file = "data/design_weights.rds")
  
    implist2 = pbapply::pblapply(1:length(implist), function(m){
      df_m = wgts_list[[m]] %>% dplyr::left_join(implist[[m]], by = c("ResponseId",".imp"))
      return(df_m)
    })
    
    readr::write_rds(implist2, file = "ShinyDisparitiesNE22/data/implist2.rds")
    
    
    agreed_implist = pbapply::pblapply(1:length(implist), function(m){
      return(wgts_list[[m]] %>% dplyr::left_join(implist[[m]] %>% get_derived_vars() %>% ETL_agreed_vars(), by = c("ResponseId",".imp")))
    })
    
    readr::write_rds(agreed_implist, file = "ShinyDisparitiesNE22/data/agreed_implist.rds")
    
      
    
      
      
    poolfileloc = "C:/Users/marcu/Dropbox/UNMC/phase-2/KidsightsDashboard/DashboardNE22/KidsightDashboardNE22/data/implist_pool_of_NE_microdata.rds"
    pool_list = readr::read_rds(file = poolfileloc)
    

check_wgts_df<- pbapply::pblapply(1:length(implist2), function(m){
  unmc_m = implist2[[m]] %>% 
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
      source = "UNMC")
  
  
  acs_m = pool_list[[m]] %>% 
    dplyr::filter(source == "ACS") %>% 
    to_acs() 
  
  ret_m = lapply(0:5, function(yr_x){
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
      dplyr::select(ResponseId, wgt, all_of(targets_acs_mx$variable)) %>% 
      tidyr::pivot_longer(aa:white, names_to = "variable", values_to ="x") %>% 
      dplyr::group_by(variable) %>% dplyr::summarise(est_raw = mean(x), est_wadj = weighted.mean(x, wgt)) %>% 
      dplyr::ungroup() %>% dplyr::mutate(sc_age_years = yr_x)
    
    targets_acs_mx = targets_acs_mx %>% dplyr::left_join(unmc_mx, by = c("sc_age_years","variable")) %>% dplyr::mutate(.imp = m)
    
    return(targets_acs_mx)
    
  }) %>% dplyr::bind_rows()
}) %>% 
  dplyr::bind_rows()


hi = check_wgts_df %>% 
  dplyr::group_by(sc_age_years, variable) %>% 
  dplyr::summarise(pcts = mean(pcts), pcts_se = mean(pcts_se) + sqrt(var(pcts_se)), est_raw = mean(est_raw), est_wadj = mean(est_wadj)) %>% 
  dplyr::rename(category = variable)

    
hi = 
  hi %>% dplyr::mutate(
    variable = NA,
    variable = ifelse(category %in% c("lths","hs","aa","ba","gtba"), "Parent's Education", variable), 
    variable = ifelse(category %in% paste0("fpl",0:4), "Federal Poverty Level (FPL)", variable), 
    variable = ifelse(category %in% c("api", "black", "native","white","other","twoplus", "hisp"), "Child's Race/Ethnicity", variable) 
  ) %>% 
  dplyr::mutate(variable= as.factor(variable), category = as.factor(category))



var_vec = c("Parent's Education","Federal Poverty Level (FPL)","Child's Race/Ethnicity")



pdf("Raking sanity check.pdf")
for(x in 1:length(var_vec)){
  plt <-  ggplot(hi %>% dplyr::filter(variable == var_vec[x])) +
    geom_errorbar(aes(x=sc_age_years, y = pcts, ymin = pcts-1.96*pcts_se, ymax = pcts+1.96*pcts_se)) +
    geom_point(aes(x=sc_age_years, y = est_raw), col = "darkred", size = 4, alpha = .5) +
    geom_point(aes(x=sc_age_years, y = est_wadj), col = "darkgreen", size = 4, alpha = .5) +
    facet_grid(category~.) +
    labs(x = "Child's Age Bin", title = var_vec[x]) + 
    theme_bw()
  print(plt)
}
dev.off()


from_vec = c("lths","hs","aa","ba")
to_vec = c(1:4)
labs_vec = c("Less than High School Graduate", "High School Graduate (including Equivalency)", "Some College or Associate's Degree", "College Degree")

from_vec = c(from_vec, paste0("fpl",0:4))
to_vec = c(to_vec, 5:9)
labs_vec = c(labs_vec, "0-99% FPL", "100-199% FPL", "200-299% FPL", "300-399% FPL", "400+% FPL")

from_vec = c(from_vec, "api","black","native", "white", "other", "twoplus", "hisp")
to_vec = c(to_vec, 10:16)
labs_vec = c(labs_vec,"Asian or PI, non-Hisp.", "Black, non-Hisp.", "Native, non-Hisp.", "White, non-Hisp.", "Other, non-Hisp.", "Two or More, non-Hisp.", "Hispanic")

hi = hi %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(category != "nohisp") %>% 
  dplyr::mutate(label = plyr::mapvalues(as.character(category), from = from_vec, to = to_vec) %>% as.numeric()) %>% 
  dplyr::arrange(sc_age_years, label) 

hi = hi %>% 
  dplyr::mutate(label = label %>% ordered(labels = labs_vec), 
                sc_age_years = sc_age_years %>% ordered(labels = c("0-11 mo.", "12-23 mo.", "24-35 mo", "36-47 mo", "48-59 mo.", "60-71 mo.")))


bue= hi %>% dplyr::relocate(pcts_se) %>% 
  tidyr::pivot_longer(cols = pcts:est_wadj, names_to = "type", values_to = "est") %>% 
  dplyr::mutate(pcts_se = ifelse(type!="pcts", NA, pcts_se)) %>% 
  dplyr::relocate(-pcts_se) %>% 
  dplyr::mutate(source = plyr::mapvalues(type, from = c("pcts","est_raw","est_wadj"), to = c(3,1:2)) %>% 
                  ordered(labels = c("Unweighted Sample", "Weighted Sample", "ACS-5 (2018-2022)")))

library(ggthemes)
thm = theme_bw() 
thm$legend.position = "bottom"
thm$plot.title.position = "panel"
thm$plot.title =  element_text(hjust = 0.5, size = 20)


plt_a = bue %>% dplyr::filter(variable == "Parent's Education") %>% 
  ggplot(aes(x = label, y = 100*est, col = source, fill = source)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 2/3, show.legend = F) +
  facet_wrap(sc_age_years~.)  +
  coord_flip() + 
  labs(title = "(A)", subtitle = "Parent's Education", y = "Estimate (%)", x= NULL) + 
  thm +
  ggthemes::scale_colour_colorblind() + ggthemes::scale_fill_colorblind()
plt_a

plt_b = bue %>% dplyr::filter(variable == "Federal Poverty Level (FPL)") %>% 
  ggplot(aes(x = label, y = 100*est, col = source, fill = source)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 2/3, show.legend = F) +
  facet_wrap(sc_age_years~.)  +
  coord_flip() + 
  labs(title = "(B)", subtitle = "Federal Poverty Level (FPL)", y = "Estimate (%)", x= NULL) + 
  thm +
  ggthemes::scale_colour_colorblind() + ggthemes::scale_fill_colorblind()
plt_b

plt_c = bue %>% dplyr::filter(variable == "Child's Race/Ethnicity") %>% 
  ggplot(aes(x = label, y = 100*est, col = source, fill = source)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 2/3) +
  facet_wrap(sc_age_years~.)  +
  coord_flip() + 
  labs(title = "(C)", subtitle = "Child's Race/Ethnicity", y = "Estimate (%)", x= NULL) + 
  thm +
  ggthemes::scale_colour_colorblind() + ggthemes::scale_fill_colorblind()


library(gridExtra)
lm = matrix(c(1, 1, 1, 2, 2, 2,
              1, 1, 1, 2, 2, 2,
              1, 1, 1, 2, 2, 2,
              1, 1, 1, 2, 2, 2,
              NA,3,3,3,3,NA, 
              NA,3,3,3,3,NA, 
              NA,3,3,3,3,NA, 
              NA,3,3,3,3,NA, 
              NA,3,3,3,3,NA), byrow = T, ncol = 6)

plt = grid.arrange(plt_a,plt_b, plt_c, layout_matrix = lm)
fact = 1
ggsave(plot = plt, filename = "C:/Users/marcu/OneDrive - University of Nebraska Medical Center/Documents - Kidsights Data/General/Publications/Disparities Paper/Tables and Figures/Supplemental Materials/Supplemental Plot 1.png", 
       height = fact*8.5, width = fact*11, units = "in")

label_agreed<-function(df_in){
 
  library(sjlabelled)
  library(sjmisc)
  
  df_in = df_in %>% 
    dplyr::mutate(
        troublebasics = set_labels(troublebasics, labels = c("Never","Rarely","Somewhat Often","Often"))
    )
  
  df_in = df_in %>% 
    dplyr::mutate(
        foodinsuff = set_labels(foodinsuff, labels = c("We could always afford to eat good nutritious meals", "We could always afford enough to eat but not always the kinds of food we should eat", "Sometimes or often we could not afford enough to eat"))
    )
  
  df_in = df_in %>% 
    dplyr::mutate(
        fpl_cat = set_labels(fpl_cat, labels = c("0-99% FPL","100-199% FPL", "200-299% FPL", "300-399% FPL", "400+% FPL"))
    )
  
  df_in = df_in %>% dplyr::rename(imp = .imp)
  
  return(df_in)
}

agreed_implist = 
  pbapply::pblapply(1:length(agreed_implist), function(m){
    
      agreed_implist[[m]] %>% label_agreed() %>% dplyr::mutate(raceG = as.factor(raceG))
    
})


tokimia = agreed_implist %>% dplyr::bind_rows()

readr::write_rds(tokimia, "data/tokimia.rds")

library(foreign)
write.dta(tokimia, "to_kimia_Disparities_5Dec2024.dta")
