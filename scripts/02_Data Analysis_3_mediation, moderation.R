###' ###########################################################################
###' 
###' Project(project name): Graduation Thesis
###' 
###' Category(stage in the project): data analysis
###' 
###' Task(specific task in the category): mediation, moderation
###' 
###' Data(data source): `survey data(independent variable)`
###' 
###' Date: 2022-05-31
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

install.packages("semTools")

### Call libraries
library(tidyverse); library(readxl); library(psy); library(psych); library(ggplot2); library(gridExtra)
library(moments); library(SciViews); library(lavaan); library(Hmisc); library(car); library(predict3d)
library(ltm); library(knitr); library(pastecs); library(mvnormtest); library(semPlot); library(semTools); library(processR)



###' ###########################################################################'
###' 
###' Import survey items
###' 
###' 

### Set file path
file_path <- file.path(data_dir, "01_coding_05_full_1234.csv")
df <- read_csv(file = file_path) %>% tibble()


#### Check variable types
head(df)
nrow(df)
df_na <- na.omit(df)
nrow(df_na)
names(df_na)

lapply(df_na, class)
suppressWarnings(df_na$GPA <- as.numeric(df_na$GPA))



###' ###########################################################################'
###' 
###' Import survey items
###' 
###' 

#### Check variable

### norm test
mshapiro.test(t(df_na[,c(2:101)])) 

### 1. Personal Relationship bind
R_friend_famil = cbind(R_friend_famil_1,R_friend_famil_2,R_friend_famil_3)
R_friend_credit = cbind(R_friend_credit_1,R_friend_credit_2,R_friend_credit_3)
R_friend_respec = cbind(R_friend_respect_1,R_friend_respect_2,R_friend_respect_3)

R_prof_famil = cbind(R_prof_famil_1,R_prof_famil_2,R_prof_famil_3)
R_prof_credit = cbind(R_prof_credit_1,R_prof_credit_2,R_prof_credit_3)
R_prof_respect = cbind(R_prof_respect_1,R_prof_respect_2,R_prof_respect_3)

R_parent_famil = cbind(R_parent_famil_1,R_parent_famil_2,R_parent_famil_3)
R_parent_credit = cbind(R_parent_credit_1,R_parent_credit_2,R_parent_credit_3)
R_parent_respect = cbind(R_parent_respect_1,R_parent_respect_2,R_parent_respect_3)


R_friend = cbind(R_friend_famil + R_friend_credit + R_friend_respec)
R_prof = cbind(R_prof_famil + R_prof_credit + R_prof_respect)
R_parent = cbind(R_parent_famil + R_parent_credit + R_parent_respect)

PersonalR <- cbind(R_friend+R_prof+R_parent)


### 2. EP bind
p1 = cbind(p11,p12,p13)
p2 = cbind(p21,p22,p23)
p3 = cbind(p31,p32,p33)
p = cbind(p1+p2+p3)

### 3. EO bind
id = cbind(id1,id2,id3,id4)

ad1 = cbind(ad11,ad12,ad13)
ad2 = cbind(ad21,ad22,ad23)
ad3 = cbind(ad31,ad32,ad33)
ad4 = cbind(ad41,ad42,ad43)
ad = cbind(ad1 + ad2 + ad3 + ad4)

pd = cbind(pd1,pd2,pd3,pd4)

eo = cbind(id + ad + pd)



###' ###########################################################################'
###' 
###' Normality test
###' 
###' 

