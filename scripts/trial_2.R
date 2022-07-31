###' ###########################################################################
###' 
###' Project(project name): Graduation Thesis
###' 
###' Category(stage in the project): Data management
###' 
###' Task(specific task in the category): Import raw dataset and cleaning: Independent Variable
###' 
###' Data(data source): `survey data(independent variable)`
###' 
###' Date: 2022-01-26
###' 
###' Author: Hyemin Park(`hyemin.park@snu.ac.kr`)
###' 
###'

###' ###########################################################################
###' 
###' Basic settings
###' 
###' 

### Start with clean state
gc(); rm(list=ls())


### Set working directory and data directory
work_dir <- c("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/GraduationThesis")
data_dir <- file.path(work_dir, "datasets")


### Call libraries
library(tidyverse); library(readxl); library(psy); library(psych); library(ggplot2); library(gridExtra); library(SEMsens)
library(moments); library(SciViews); library(moments); library(lavaan); library(Hmisc); library(car)
library(ltm); library(knitr); library(pastecs); library(mvnormtest); library(semPlot); library(semTools)




###' ###########################################################################'
###' 
###' Mediated Moderation Effect Analysis
###' 
###'




table(df_na_b$gender)
sd(df_na_b$gender)

df_na_b_region <- df_na_b %>%
  filter(Hregion == 1 | 2 | 3)
table(df_na_b$Hregion)



set.seed(111)
df_na_b.mod.med_1 <- indProd(df_na_b, var1=c("R_friend", "R_prof", "R_parent"),
                             var2="Hregion", match=FALSE,
                             meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(df_na_b.mod.med_1)

mod.med_1 <- 
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
  
  Personalgender =~ R_friend.Hregion + R_prof.Hregion + R_parent.Hregion
   
  AcademicR ~ a*PersonalR +  C4*Grade +  C1*gender
  UnivEduAchive ~ c1*PersonalR + c2*Hregion + c3*Personalgender + b*AcademicR +  C4*Grade +  C1*gender

   
   indirect := a*b
   
   direct_H := c1 + c2*1
   direct_M := c1 + c2*2
   direct_L := c1 + c2*3
   
   total_H := direct_H + indirect
   total_M := direct_M + indirect
   total_L := direct_L + indirect'


set.seed(111)
fit.mod.med_1 <- sem(model=mod.med_1, data=df_na_b.mod.med_1, se="bootstrap", bootstrap=10)
summary(fit.mod.med_1, standardized=TRUE)
parameterEstimates(fit.mod.med_1, standardized=TRUE)





###' ###########################################################################'
###' 
###' Moderated Mediation Effect Analysis
###' 
###'


### only for PersonalR_2


set.seed(111)
df_na_b.mod.med_2 <- indProd(df_na_b, var1=c("R_friend", "R_prof", "R_parent"),
                             var2="gender", match=FALSE,
                             meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(df_na_b.mod.med_2)

mod.med_2 <- 
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
  
  Personalgender =~ R_friend.gender + R_prof.gender + R_parent.gender
   
  AcademicR ~ a1*PersonalR + a2*gender + a3*Personalgender
  UnivEduAchive ~ c*PersonalR + b*AcademicR

   
   
   indirect_H := a*b
   
   direct_H := a1 + c3*1
   direct_M := a3 + c3*2
   
   total_H := direct_H + indirect_H
   total_M := direct_M + indirect_H'


set.seed(111)
fit.mod.med_2 <- sem(model=mod.med_2, data=df_na_b.mod.med_2, se="bootstrap", bootstrap=10)
summary(fit.mod.med_2, standardized=TRUE)

parameterEstimates(fit.mod.med_2, standardized=TRUE)







### only for AcademicR_3
set.seed(111)
df_na_b.mod.med_3 <- indProd(df_na_b_type, var1=c("Resili_SelfEffi", "Resili_SituJuge", "Resili_Resource", "Resili_Vital", "Resili_FutureOrien"),
                             var2="Htype", match=FALSE,
                             meanC=TRUE, residualC=FALSE, doubleMC=TRUE)



names(df_na_b.mod.med_3)

mod.med_3 <- 
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
  
  Academicgender =~ Resili_SelfEffi.Htype + Resili_SituJuge.Htype + Resili_Resource.Htype + Resili_Vital.Htype  + Resili_FutureOrien.Htype
   
  AcademicR ~ a*PersonalR + C4*Grade  + C1*gender
  UnivEduAchive ~ c*PersonalR + b1*AcademicR + b2*Htype + b3*Academicgender + C4*Grade + C1*gender

   
   M_D_H := b3 + b2*1
   M_D_M := b3 + b2*2
   M_D_L := b3 + b2*3
   M_D_E := b3 + b2*6
   
   indirect_H := M_D_H*a
   indirect_M := M_D_M*a
   indirect_L := M_D_L*a
   indirect_E := M_D_E*a
   
   direct := c
   
   total_H := c + indirect_H
   total_M := c + indirect_M
   total_L := c + indirect_L'



fit.mod.med_3 <- sem(model=mod.med_3, data=df_na_b.mod.med_3)
summary(fit.mod.med_3, standardized=TRUE)
parameterEstimates(fit.mod.med_3, standardized=TRUE)





###  for PersonalR and AcademicR_4
set.seed(111)
df_na_b.mod.med_4 <- indProd(df_na_b_type, var1=c("R_friend", "R_prof", "R_parent", "Resili_SelfEffi", "Resili_SituJuge", "Resili_Resource", "Resili_Vital", "Resili_FutureOrien"),
                             var2="Htype", match=FALSE,
                             meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(df_na_b.mod.med_4)

mod.med_4 <- 
  '# measurement model
  PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
              
  # interaction term
  Personalgender =~ R_friend.Htype + R_prof.Htype + R_parent.Htype
  Academicgender =~ Resili_SelfEffi.Htype + Resili_SituJuge.Htype + Resili_Resource.Htype + Resili_Vital.Htype  + Resili_FutureOrien.Htype

            
  # regressions
  AcademicR ~ a1*PersonalR + a2*Htype + a3*Personalgender + C4*Grade + C3*gender
  UnivEduAchive ~ c1*PersonalR + c2*Htype + c3*Personalgender + b1*AcademicR + b3*Academicgender  + C4*Grade + C3*gender


   I_M_H := a3 + a2*1
   I_M_M := a3 + a2*2
   I_M_L := a3 + a2*3
   
   I_D_H := c3 + c2*1
   I_D_M := c3 + c2*2
   I_D_L := c3 + c2*3
   
   M_D_H := b3 + c2*1
   M_D_M := b3 + c2*2
   M_D_L := b3 + c2*2
   
   indirect_H := (a3 + a2*1)*(b3 + c2*1)
   indirect_M := (a3 + a2*2)*(b3 + c2*2)
   indirect_L := (a3 + a2*3)*(b3 + c2*3)
   
   total_H := indirect_H + I_D_H
   total_M := indirect_M + I_D_M
   total_L := indirect_L + I_D_L'



fit.mod.med_4 <- sem(model=mod.med_4, data=df_na_b.mod.med_4)
summary(fit.mod.med_4, standardized=TRUE)
parameterEstimates(fit.mod.med_4, standardized=TRUE)





set.seed(111)
df_na_b.mod.med_5 <- indProd(df_na_b, var1=c("R_friend", "R_prof", "R_parent", "Resili_SelfEffi", "Resili_SituJuge", "Resili_Resource", "Resili_Vital", "Resili_FutureOrien"),
                             var2="gender", match=FALSE,
                             meanC=TRUE, residualC=FALSE, doubleMC=TRUE)

mod.model_5 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
  
  Personalgender =~ R_friend.gender + R_prof.gender + R_parent.gender
  Academicgender =~ Resili_SelfEffi.gender + Resili_SituJuge.gender + Resili_Resource.gender + Resili_Vital.gender + Resili_FutureOrien.gender
   
  AcademicR ~ a1*PersonalR + a2*gender + a3*Personalgender
  UnivEduAchive ~ c1*PersonalR + c2*gender + b1*AcademicR + b3*Academicgender 
  

   
   M_D_F := b3 + c2*1
   M_D_M := b3 + c2*2
   
   
   indirect_F := (a1)*(b3 + c2*1)
   indirect_M := (a1)*(b3 + c2*2)
   
   total_F := indirect_F + c1
   total_M := indirect_M + c1'



fit.mod.med_5 <- sem(model=mod.model_5, data=df_na_b.mod.med_5)
summary(fit.mod.med_5, standardized=TRUE)
parameterEstimates(fit.mod.med_5, standardized=TRUE)
