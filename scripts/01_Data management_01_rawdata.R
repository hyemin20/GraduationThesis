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
###' Date: 2022-04-24
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
library(tidyverse); library(readxl); library(psy); library(psych); library(ggplot2); library(gridExtra)
library(moments); library(SciViews); library(moments)




###' ###########################################################################'
###' 
###' Import survey items
###' 
###' 

### Set file path
df <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/GraduationThesis/datasets/01_coding_06_mean_01_sub.csv", 
               header = TRUE, stringsAsFactors = FALSE) %>% tibble()


#### Check variable types
head(df)
nrow(df)
df_na <- na.omit(df)
nrow(df_na)
names(df)

df_indepen_n <- df_na[,2:5]
df_indepen_w <- df_na[,6:9]
df_indepen_v <- df_na[,10:13]
df_indepen_n$R_friend <- as.numeric(df_indepen_n$R_friend)
df_indepen_n$R_prof <- as.numeric(df_indepen_n$R_prof)
df_indepen_n$R_parent <- as.numeric(df_indepen_n$R_parent)
df_indepen_n$R_total <- as.numeric(df_indepen_n$R_total)

df_medi <- df_na[,14:19]
df_medi$Resili_SelfEffi <- as.numeric(df_medi$Resili_SelfEffi)
df_medi$Resili_SituJuge <- as.numeric(df_medi$Resili_SituJuge)
df_medi$Resili_Resource <- as.numeric(df_medi$Resili_Resource)
df_medi$Resili_Vital <- as.numeric(df_medi$Resili_Vital)
df_medi$Resili_FutureOrien <- as.numeric(df_medi$Resili_FutureOrien)
df_medi$Resili_total <- as.numeric(df_medi$Resili_total)


df_depen_1 <- df_na[,20:23]
df_depen_1$UnivEduSatis_liberalarts <- as.numeric(df_depen_1$UnivEduSatis_liberalarts)
df_depen_1$UnivEduSatis_major <- as.numeric(df_depen_1$UnivEduSatis_major)
df_depen_1$UnivEduSatis_infra <- as.numeric(df_depen_1$UnivEduSatis_infra)
df_depen_1$UnivEduSatis_total <- as.numeric(df_depen_1$UnivEduSatis_total)


df_depen_2 <- df_na[,24]


df_info <- df_na[,c(1,25:33)]
table(df_info$Grade)
table(df_info$gender)
table(df_info$Depart)
table(df_info$Major)
table(df_info$Htype)
table(df_info$Hregion)
table(df_info$Residence)



###' ###########################################################################'
###' 
###' Import survey items
###' 
###' 

#### Check variable

map(df_indepen_n,summary)
map(df_medi,summary)
map(df_depen_1,summary)
map(df_depen_2,summary)




###' ###########################################################################'
###' 
###' cronbach's alpha
###' 
###'

names(df_allQ)
A <- df_allQ[,2:29]
B <- df_allQ[,44:88]
c <- df_allQ[,91:102]

alpha(A)
cronbach(A)
cronbach(df_medi)
cronbach(df_depen_1)



ROKmodel <- 'c1 =~ c11+c12+c13+c14+c15+c16
          c2 =~ c21+c22+c23+c24+c25+c26
          c3 =~ c31+c32+c33+c34+c35+c36
          c4 =~ c41+c42+c43+c44+c45+c46
          c5 =~ c51+c52+c53+c54+c55+c56'
ROKfit <- cfa(ROKmodel, data = ROKdata, estimator = "MLM")
lavInspect(ROKfit,"cov.lv")
summary(ROKfit, fit.measures=TRUE, standardized = TRUE, rsquare = TRUE) 

#reliability
ROK_R <- reliability(ROKfit,return.total = TRUE)
ROK_R


alpha(data1)

cfa.model_1 <-
  'total_1=~cog_1+emo_1+fin_1
  cog_1 =~ cog1+cog2+cog3+cog4+cog5
  emo_1 =~ emo1+emo2+emo3+emo4+emo5
  fin_1 =~ fin1+fin2+fin3+fin4+fin5
total_1~~total_1
cog1~~cog2
cog2~~cog3
cog3~~cog4
cog4~~cog5
emo1~~emo2
emo2~~emo3
emo3~~emo4
emo4~~emo5
fin1~~fin2
fin2~~fin3
fin3~~fin4
fin4~~fin5'

cfa.out_1 <- cfa(cfa.model_1, data1)
summary(cfa.out_1, fit.measures=T, standardized=T)
summary(cfa.out_1, fit.measures=T, standardized=T, rsquare=T)
