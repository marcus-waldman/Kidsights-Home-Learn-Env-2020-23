get_cahmi_xwalk<-function(cahmi_2016_2020){

  ###### Create the Nebraska analyzable data set data set #####
  
  afemale_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$a1_sex),
        asex = sjlabelled::get_labels(cahmi_2016_2020$a1_sex, values = "n") %>% names() %>%  as.numeric() 
      ) %>% 
        mutate(afemale = ifelse(asex==2,1,0)), 
      val_labs = c(Male=0,Female=1), 
      var_labs = "(Recode) Female adult"
    )
  
  foodstamp_rc_xwalk = 
    list(
      xwalk = 
        data.frame(
          orig_labels = sjlabelled::get_labels(cahmi_2016_2020$foodstamp),
          foodstamp = sjlabelled::get_labels(cahmi_2016_2020$foodstamp, values = "n") %>% names() %>%  as.numeric() 
        ) %>% 
        mutate(foodstamp_rc = ifelse(foodstamp==1,1,0)),
      val_labs = c(No=0,Yes=1), 
      var_lab = paste("(Recode)", sjlabelled::get_label(cahmi_2016_2020$foodstamp))
    )
  
  
  govhealthc_rc_xwalk = 
    list(
      xwalk = 
        data.frame(
          orig_labels = sjlabelled::get_labels(cahmi_2016_2020$govhealthc),
          govhealthc = sjlabelled::get_labels(cahmi_2016_2020$govhealthc, values = "n") %>% names() %>%  as.numeric() 
        ) %>% 
        mutate(govhealthc_rc = ifelse(govhealthc==1,1,0)),
      val_labs = c(No=0,Yes=1), 
      var_lab = paste("(Recode)", sjlabelled::get_label(cahmi_2016_2020$govhealthc))
    )
  
  wic_rc_xwalk = 
    list(
      xwalk = 
        data.frame(
          orig_labels = sjlabelled::get_labels(cahmi_2016_2020$wic),
          wic = sjlabelled::get_labels(cahmi_2016_2020$wic, values = "n") %>% names() %>%  as.numeric() 
        ) %>% 
        mutate(wic_rc = ifelse(wic==1,1,0)),
      val_labs = c(No=0,Yes=1), 
      var_lab = paste("(Recode)", sjlabelled::get_label(cahmi_2016_2020$wic))
    )
  
  cashass_rc_xwalk = 
    list(
      xwalk = 
        data.frame(
          orig_labels = sjlabelled::get_labels(cahmi_2016_2020$cashass),
          cashass = sjlabelled::get_labels(cahmi_2016_2020$cashass, values = "n") %>% names() %>%  as.numeric() 
        ) %>% 
        mutate(cashass_rc = ifelse(cashass==1,1,0)),
      val_labs = c(No=0,Yes=1), 
      var_lab = paste("(Recode)", sjlabelled::get_label(cahmi_2016_2020$cashass))
    )
  
  mealfree_rc_xwalk = 
    list(
      xwalk = 
        data.frame(
          orig_labels = sjlabelled::get_labels(cahmi_2016_2020$mealfree),
          mealfree = sjlabelled::get_labels(cahmi_2016_2020$mealfree, values = "n") %>% names() %>%  as.numeric() 
        ) %>% 
        mutate(mealfree_rc = ifelse(mealfree==1,1,0)),
      val_labs = c(No=0,Yes=1), 
      var_lab = paste("(Recode)", sjlabelled::get_label(cahmi_2016_2020$mealfree))
    )
  
  medicare_rc_xwalk = 
    list(
      xwalk = 
        data.frame(
          orig_labels = c("No", "Yes"),
          medicare = c(1, 2) 
        ) %>% 
        mutate(medicare_rc = medicare-1 %>% as.integer()),
      val_labs = c(No=0,Yes=1), 
      var_lab = "(Recode) Health insurance through Medicare"
    )
  
  edu4_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$a1_grade),
        grade = sjlabelled::get_labels(cahmi_2016_2020$a1_grade, values = "n") %>% names() %>%  as.numeric(), 
        edu4 = c(0,0,1,2,2,2,3,3,3)
      ), 
      val_labs = c("Less than HS Diploma/GED"=0,"HS Diploma/GED"=1,"Some College, AA/AS, or Vocational/Trade/etc."=2,"BA/BS or Higher"=3), 
      var_lab = "Educational attainment (4 cat.; derived)"
    )
  
  
  married_xwalk = 
    list(xwalk = data.frame(
      orig_labels = sjlabelled::get_labels(cahmi_2016_2020$a1_marital), 
      marital = sjlabelled::get_labels(cahmi_2016_2020$a1_marital, values = "n") %>% names() %>%  as.numeric() ) %>% 
        mutate(married = ifelse(marital==1,1,0)), 
      val_labs = c(No=0,Yes=1), 
      var_lab = "Married")
  
  
  parent_xwalk = 
    list(
      xwalk = 
        data.frame(
          orig_labels = sjlabelled::get_labels(cahmi_2016_2020$a1_relation), 
          relation = sjlabelled::get_labels(cahmi_2016_2020$a1_relation, values = "n") %>% names() %>% as.numeric() 
        ) %>% 
        mutate(parent = ifelse(relation==1|relation==2|relation==4,1,0)),
      val_labels = c(No=0,Yes=1), 
      var_lab = "Parent of child (biological, adoptive, step-, or foster)"
    )
  
  female_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$sex),
        sex = sjlabelled::get_labels(cahmi_2016_2020$sex, values = "n") %>% names() %>%  as.numeric() 
      ) %>% 
        mutate(female = ifelse(sex==2,1,0)), 
      val_labs = c(Male=0,Female=1), 
      var_labs = "(Recode) Female child"
    )
  
  
  acedivorce_rc_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$acedivorce),
        acedivorce = sjlabelled::get_labels(cahmi_2016_2020$acedivorce, values = "n") %>% names() %>%  as.numeric() 
      ) %>% 
        mutate(acedivorce_rc = ifelse(acedivorce==2,0,1)), 
      val_labs = c(No=0,Yes=1), 
      var_labs = paste0("(Recode) ", sjlabelled::get_label(cahmi_2016_2020$acedivorce))
    )
  
  
  acedeath_rc_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$acedeath),
        acedeath = sjlabelled::get_labels(cahmi_2016_2020$acedeath, values = "n") %>% names() %>%  as.numeric() 
      ) %>% 
        mutate(acedeath_rc = ifelse(acedeath==2,0,1)), 
      val_labs = c(No=0,Yes=1), 
      var_labs = paste0("(Recode) ", sjlabelled::get_label(cahmi_2016_2020$acedeath))
    )
  
  acejail_rc_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$acejail),
        acejail = sjlabelled::get_labels(cahmi_2016_2020$acejail, values = "n") %>% names() %>%  as.numeric() 
      ) %>% 
        mutate(acejail_rc = ifelse(acejail==2,0,1)), 
      val_labs = c(No=0,Yes=1), 
      var_labs = paste0("(Recode) ", sjlabelled::get_label(cahmi_2016_2020$acejail))
    )
  
  acedv_rc_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$acedv),
        acedv = sjlabelled::get_labels(cahmi_2016_2020$acedv, values = "n") %>% names() %>%  as.numeric() 
      ) %>% 
        mutate(acedv_rc = ifelse(acedv==2,0,1)), 
      val_labs = c(No=0,Yes=1), 
      var_labs = paste0("(Recode) ", sjlabelled::get_label(cahmi_2016_2020$acedv))
    )
  
  aceviolence_rc_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$aceviolence),
        aceviolence = sjlabelled::get_labels(cahmi_2016_2020$aceviolence, values = "n") %>% names() %>%  as.numeric() 
      ) %>% 
        mutate(aceviolence_rc = ifelse(aceviolence==2,0,1)), 
      val_labs = c(No=0,Yes=1), 
      var_labs = paste0("(Recode) ", sjlabelled::get_label(cahmi_2016_2020$aceviolence))
    )
  
  acementalill_rc_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$acementalill),
        acementalill = sjlabelled::get_labels(cahmi_2016_2020$acementalill, values = "n") %>% names() %>%  as.numeric() 
      ) %>% 
        mutate(acementalill_rc = ifelse(acementalill==2,0,1)), 
      val_labs = c(No=0,Yes=1), 
      var_labs = paste0("(Recode) ", sjlabelled::get_label(cahmi_2016_2020$acementalill))
    )
  
  aceaddict_rc_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$aceaddict),
        aceaddict = sjlabelled::get_labels(cahmi_2016_2020$aceaddict, values = "n") %>% names() %>%  as.numeric() 
      ) %>% 
        mutate(aceaddict_rc = ifelse(aceaddict==2,0,1)), 
      val_labs = c(No=0,Yes=1), 
      var_labs = paste0("(Recode) ", sjlabelled::get_label(cahmi_2016_2020$aceaddict))
    )
  
  aceracism_rc_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$aceracism),
        aceracism = sjlabelled::get_labels(cahmi_2016_2020$aceracism, values = "n") %>% names() %>%  as.numeric() 
      ) %>% 
        mutate(aceracism_rc = ifelse(aceracism==2,0,1)), 
      val_labs = c(No=0,Yes=1), 
      var_labs = paste0("(Recode) ", sjlabelled::get_label(cahmi_2016_2020$aceracism))
    )
  
  care10hr_rc_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$care10hr),
        care10hr= sjlabelled::get_labels(cahmi_2016_2020$care10hr, values = "n") %>% names() %>%  as.numeric() 
      ) %>% 
        mutate(care10hr_rc = ifelse(care10hr==2,0,1)), 
      val_labs = c(No=0,Yes=1), 
      var_labs = paste0("(Recode) ", sjlabelled::get_label(cahmi_2016_2020$care10hr))
    )
  
  race4_char_xwalk = 
    list(
      xwalk = data.frame(
        orig_labels = sjlabelled::get_labels(cahmi_2016_2020$race4),
        race4= sjlabelled::get_labels(cahmi_2016_2020$race4, values = "n") %>% names() %>%  as.numeric() 
      ) %>% 
        mutate(race4_char= orig_labels), 
      val_labs = NULL, 
      var_labs = paste0("(chr) ", sjlabelled::get_label(cahmi_2016_2020$race4))
    )
  
  xwalk_cahmi = list(
    afemale = afemale_xwalk,
    foodstamp_rc = foodstamp_rc_xwalk,
    govhealthc_rc = govhealthc_rc_xwalk, 
    wic_rc = wic_rc_xwalk, 
    cashass_rc = cashass_rc_xwalk, 
    mealfree_rc = mealfree_rc_xwalk,
    medicare_rc = medicare_rc_xwalk,
    edu4 = edu4_xwalk,
    married = married_xwalk,
    parent = parent_xwalk, 
    female = female_xwalk, 
    acedivorce_rc = acedivorce_rc_xwalk, 
    acedeath_rc = acedeath_rc_xwalk, 
    acejail_rc = acejail_rc_xwalk, 
    acedv_rc = acedv_rc_xwalk, 
    aceviolence_rc = aceviolence_rc_xwalk, 
    acementalill_rc = acementalill_rc_xwalk,
    aceaddict_rc = aceaddict_rc_xwalk,
    aceracism_rc = aceracism_rc_xwalk, 
    race4_char = race4_char_xwalk
  )

return(xwalk_cahmi)

}
