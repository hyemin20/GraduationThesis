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
library(tidyverse); library(readxl); library(psy); library(psych); library(ggplot2); library(SEMsens)
library(SciViews); library(moments); library(lavaan); library(Hmisc); library(car); library(dplyr); library(stargazer)
library(ltm); library(knitr); library(pastecs); library(mvnormtest); library(semPlot); library(semTools)



###' ###########################################################################'
###' 
###' Import survey items
###' 
###' 

### Set file path
file_path_a <- file.path(data_dir, "01_coding_06_mean_01_sub.csv")
file_path_b <- file.path(data_dir, "01_coding_06_mean_02_total.csv")
df_a <- read_csv(file = file_path_a) %>% tibble()
df_b <- read_csv(file = file_path_b) %>% tibble()


#### Check variable types
head(df_a)
head(df_b)

nrow(df_a)
nrow(df_b)

df_na_a <- na.omit(df_a)
df_na_b <- na.omit(df_b)

nrow(df_na_a)
nrow(df_na_b)

names(df_a)
names(df_b)


### make variable
df_indepen_n <- df_na_b[,2:5]
df_indepen_w <- df_na_b[,6:9]
df_indepen_v <- df_na_b[,10:13]
df_medi <- df_na_b[,14:19]
df_depen_1 <- df_na_b[,20:23]
df_depen_2 <- df_na_b[,24:26]


df_info <- df_na_b[,c(1,27:35)]
table(df_info$Grade)
table(df_info$gender)
table(df_info$Depart)
table(df_info$Major)
table(df_info$Htype)
table(df_info$Hregion)
table(df_info$Residence)


#### Check variable

lapply(df_na_a, class)
lapply(df_na_b, class)

names(df_na_a)
names(df_na_b)



###' ###########################################################################'
###' 
###' Mediation Effect Analysis
###' 
###'

sem.med <- 
  # measurement model
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
  
  # regressions
  AcademicR ~ a*PersonalR
  UnivEduAchive ~ c*PersonalR + b*AcademicR
  
  R_friend ~~ R_prof
  R_prof ~~ R_parent
  
  # indirect effect: ab
  ab := a*b
  # total effect: c
  d := c + (a*b)'


set.seed(123)
fit.med <- sem(model=sem.med, data=df_na_b, se="bootstrap", bootstrap=10)
summary(fit.med, standardized=TRUE)


parameterEstimates(fit.med, standardized=TRUE) %>% 
  filter(op == "~" | op == ":=") %>% 
  mutate(stars=ifelse(pvalue < 0.001, "***", 
                      ifelse(pvalue < 0.01, "**", 
                             ifelse(pvalue < 0.05, "*", ""))))

semPaths(fit.med, what="std", layout="tree", edge.label.cex=1, 
         edge.color="royalblue", 
         color=list(lat="tomato", man="grey"), fade=FALSE, 
         style="lisrel", curvature=2, title = TRUE,
         rotation = 2)

a = 0.235 ; b = 0.729 
s_a = 0.045 ;  s_b = 0.352   
z = a * b / sqrt(b^2 * s_a^2 + a^2 * s_b^2)
z

2 * pnorm(z, lower.tail = F)



###' ###########################################################################'
###' 
###' Mediated Moderation Effect Analysis
###' 
###'

df_na_b.mod <- indProd(df_na_b, var1=c("R_friend", "R_prof", "R_parent"),
                                var2=c("Resili_SelfEffi", "Resili_SituJuge", "Resili_Resource", "Resili_Vital","Resili_FutureOrien"), match=FALSE,
                                meanC=TRUE, residualC=FALSE, doubleMC=TRUE)


head(df_na_b.mod)
names(df_na_b.mod)

sem.mod <- 
  "# measurement model
  PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
              
  # interaction term
  PersonalAcademic =~ R_friend.Resili_SelfEffi+R_friend.Resili_SituJuge+R_friend.Resili_Resource+R_friend.Resili_Vital+R_friend.Resili_FutureOrien+
                      R_prof.Resili_SelfEffi+R_prof.Resili_SituJuge+R_prof.Resili_Resource+R_prof.Resili_Vital+R_prof.Resili_FutureOrien+
                      R_parent.Resili_SelfEffi+R_parent.Resili_SituJuge+R_parent.Resili_Resource+R_parent.Resili_Vital+R_parent.Resili_FutureOrien
            
  # regressions
  UnivEduAchive ~ PersonalR + AcademicR + PersonalAcademic
  AcademicR ~ PersonalR
              
  # residual correlations
  R_friend ~~ R_prof
  R_prof ~~ R_parent"


fit.mod <- sem(model=sem.mod, data=df_na_b.mod)
summary(fit.mod, standardized=TRUE)
parameterEstimates(fit.mod, standardized=TRUE)



###' ###########################################################################'
###' 
###' Moderated Mediation Effect Analysis
###' 
###'


### only for PersonalR_1

sd(df_na_b$gender)
set.seed(111)

df_na_b$Gender <-
  scale(rnorm(nrow(df_na_b), mean=1, sd=0.1)*rowMeans(df_na_b[, c("R_friend", "R_prof", "R_parent")]),
        center=TRUE, scale=FALSE)



df_na_b.mod.med_1 <- indProd(df_na_b, var1=c("R_friend", "R_prof", "R_parent"),
                       var2="Gender", match=FALSE,
                       meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(df_na_b.mod.med_1)

mod.med_1 <- 
  "# measurement model
  PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
              
  # interaction term
  PersonalGender =~ R_friend.Gender + R_prof.Gender + R_parent.Gender
  

  # regressions
  UnivEduAchive ~ c*PersonalR + b*AcademicR
  AcademicR ~ a1*PersonalR + a2*Gender + a3*PersonalGender
  
  #  mean and variance of moderator
  Gender ~ Gender.mean*1
  Gender ~~ Gender.var*Gender
  
  # residual correlations
  R_friend ~~ R_prof
  R_prof ~~ R_parent
  

  mean.Gender := Gender.mean
  sd.Gender := sqrt(Gender.var)
  indirect.low := (a1 + a3*(Gender.mean - sqrt(Gender.var)))*b
  indirect.high := (a1 + a3*(Gender.mean + sqrt(Gender.var)))*b
  direct := c
  total.low := direct + indirect.low
  total.high := direct + indirect.high
  mod.med.a3b := a3*b"


set.seed(111)
fit.mod.med_1 <- sem(model=mod.med_1, data=df_na_b.mod.med_1, se="bootstrap", bootstrap=10)
summary(fit.mod.med_1, standardized=TRUE)
parameterEstimates(fit.mod.med_1, standardized=TRUE)



### only for AcademicR_2
set.seed(111)
df_na_b.mod.med_2 <- indProd(df_na_b, var1=c("Resili_SelfEffi", "Resili_SituJuge", "Resili_Resource", "Resili_Vital", "Resili_FutureOrien"),
                             var2="gender", match=FALSE,
                             meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(df_na_b.mod.med_2)

mod.med <- 
  "# measurement model
  PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
              
  # interaction term
  PersonalAcademic =~ R_friend.Resili_SelfEffi+R_friend.Resili_SituJuge+R_friend.Resili_Resource+R_friend.Resili_Vital+R_friend.Resili_FutureOrien+
                      R_prof.Resili_SelfEffi+R_prof.Resili_SituJuge+R_prof.Resili_Resource+R_prof.Resili_Vital+R_prof.Resili_FutureOrien+
                      R_parent.Resili_SelfEffi+R_parent.Resili_SituJuge+R_parent.Resili_Resource+R_parent.Resili_Vital+R_parent.Resili_FutureOrien
            
  # regressions
  UnivEduAchive ~ PersonalR + AcademicR + PersonalAcademic
  AcademicR ~ PersonalR
              
  # residual correlations
  R_friend ~~ R_prof
  R_prof ~~ R_parent"


fit.mod.med_2 <- sem(model=sem.mod, data=df_na_b.mod.med_2)
summary(fit.mod.med_2, standardized=TRUE)
parameterEstimates(fit.mod.med_2, standardized=TRUE)



###  for PersonalR and AcademicR_3
set.seed(111)
df_na_b.mod.med_3 <- indProd(df_na_b, var1=c("R_friend", "R_prof", "R_parent", "Resili_SelfEffi", "Resili_SituJuge", "Resili_Resource", "Resili_Vital", "Resili_FutureOrien"),
                             var2="gender", match=FALSE,
                             meanC=TRUE, residualC=FALSE, doubleMC=TRUE)
names(df_na_b.mod.med_3)

mod.med <- 
  "# measurement model
  PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
              
  # interaction term
  PersonalAcademic =~ R_friend.Resili_SelfEffi+R_friend.Resili_SituJuge+R_friend.Resili_Resource+R_friend.Resili_Vital+R_friend.Resili_FutureOrien+
                      R_prof.Resili_SelfEffi+R_prof.Resili_SituJuge+R_prof.Resili_Resource+R_prof.Resili_Vital+R_prof.Resili_FutureOrien+
                      R_parent.Resili_SelfEffi+R_parent.Resili_SituJuge+R_parent.Resili_Resource+R_parent.Resili_Vital+R_parent.Resili_FutureOrien
            
  # regressions
  UnivEduAchive ~ PersonalR + AcademicR + PersonalAcademic
  AcademicR ~ PersonalR
              
  # residual correlations
  R_friend ~~ R_prof
  R_prof ~~ R_parent"


fit.mod.med_3 <- sem(model=sem.mod, data=df_na_b.mod.med_3)
summary(fit.mod.med_3, standardized=TRUE)
parameterEstimates(fit.mod.med_3, standardized=TRUE)



###' ###########################################################################'
###' 
###' Sensitive Analysis
###' 
###'



### sensitive analysis_1
sem_model <- 
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
  UnivEduAchive ~ PersonalR + AcademicR
  AcademicR ~ PersonalR
  R_friend ~~ R_prof
  R_prof ~~ R_parent'

sen_model <- 
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
  
  PersonalR ~ phantom1*phantom
  AcademicR ~ phantom2*phantom
  
  AcademicR ~ PersonalR
  UnivEduAchive ~ PersonalR + AcademicR
  
  R_friend ~~ R_prof
  R_prof ~~ R_parent

  phantom =~ 0
  phantom ~~ 1*phantom'

sem_path <-
  'AcademicR ~ PersonalR
  UnivEduAchive ~ PersonalR + AcademicR'


sens_analysis <- sa.aco(data = df_na_b,
                 sample.cov = full,
                 sample.nobs = 243,
                 model = sem_model,
                 opt.fun = 3,
                 rate.of.conv = 0.00000001,
                 sens.model = sen_model,
                 paths = sem_path)

sens_tables <- sens.tables(sens_analysis)
sens_tables$sens.summary
sens_tables$phan.paths


### sensitive analysis_2

