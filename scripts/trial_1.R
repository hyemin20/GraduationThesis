


###' ###########################################################################'
###' 
###' Sensitive Analysis
###' 
###'

nrow(df_na_a)


### sensitive analysis_1
sem_model <- 
  # measurement model
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_new_5
  
  # regressions
  PersonalR ~ C1*Grade + C2*Hregion
  AcademicR ~ a*PersonalR + C2*Grade + C2*Hregion
  UnivEduAchive ~ c*PersonalR + b*AcademicR +  C2*Grade + C2*Hregion'


sen_model <- 
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_new_5
    
  PersonalR ~ phantom1*phantom
  AcademicR ~ phantom2*phantom
  UnivEduAchive ~ phantom3*phantom
  

  PersonalR ~ C1*Grade + C4*Hregion
  AcademicR ~ a*PersonalR + C2*Grade + C5*Hregion
  UnivEduAchive ~ c*PersonalR + b*AcademicR +  C3*Grade + C5*Hregion
  
  
  phantom =~ 0
  phantom ~~ 1*phantom'

ptab = lavaanify(model = sem_model , auto = TRUE, model.type="sem")
ptab[c(13,16,17),1:4]

my.sa <- sa.aco(model = sem_model, sens.model = sen_model,sample.cov = full,
                sample.nobs = 230, opt.fun = 3, paths = c(13,16,17), k = 5, max.iter = 1000)

sem_path <-
  'AcademicR ~ PersonalR
  UnivEduAchive ~ PersonalR + AcademicR'


sens_analysis <- sa.aco(data = df_na_b,
                        sample.cov = full,
                        sample.nobs = 230,
                        model = sem_model,
                        opt.fun = 3,
                        rate.of.conv = 0.01,
                        sens.model = sen_model,
                        paths = sem_path)

sens_tables <- sens.tables(expr = sens_analysis, sig.level = 0.05, path = TRUE, sort = TRUE)

sens_tables = lapply(sens_tables, round, digits = 3) 

sens_tables$sens.summary
sens_tables$phan.paths
sens_tables$phan.min

