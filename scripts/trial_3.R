### Call libraries
library(tidyverse); library(readxl); library(psy); library(psych); library(ggplot2); library(gridExtra); library(SEMsens)
library(moments); library(SciViews); library(moments); library(lavaan); library(Hmisc); library(car)
library(ltm); library(knitr); library(pastecs); library(mvnormtest); library(semPlot); library(semTools)

sem_model <- 
  # measurement model
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_new_5
  
  
  PersonalR ~ C1*gender
  AcademicR ~ a*PersonalR + C2*gender
  UnivEduAchive ~ c*PersonalR + b*AcademicR +  C3*gender'

fit.mod.med_2 <- sem(model=sem_model, data=df_na_b)
fitMeasures(fit.mod.med_2 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr"))
summary(fit.mod.med_2, standardized=TRUE)


a <- df_na_b %>%
  filter(Hregion == 1)AcademicR
b <- df_na_b %>%
  filter(Grade == 2)
c <- df_na_b %>%
  filter(Hregion == 3)
d <- df_na_b %>%
  filter(Hregion == 4)
table(df_na_b$gender)
lapply(a, summary)

11.889, 19.00, 11.25/3.631
11.667 18.44 11.00/3.570
names(a)
t.test(a$R_total, b$R_total)
t.test(a$Resili_total, b$Resili_total)
t.test(a$UnivEduSatis_total, b$UnivEduSatis_total)
t.test(a$GPA_new_5, b$GPA_new_5)


table(R)
mean(a$R_total/3)
mean(b$R_total/3)
mean(c$R_total/3)
mean(d$R_total/3)

mean(a$Resili_total/5)
mean(b$Resili_total/5)
mean(c$Resili_total/5)
mean(d$Resili_total/5)

mean(a$UnivEduSatis_total/3)
mean(b$UnivEduSatis_total/3)
mean(c$UnivEduSatis_total/3)
mean(d$UnivEduSatis_total/3)


mean(a$GPA_new_5)
mean(b$GPA_new_5)
mean(c$GPA_new_5)
mean(d$GPA_new_5)
