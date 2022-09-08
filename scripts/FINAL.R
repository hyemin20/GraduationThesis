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

dev.off()

###' ###########################################################################'
###' 
###' Import survey items
###' 
###' 

### Set file path
file_path <- file.path(data_dir, "01_coding_05_full_1234_O.csv")
file_path_a <- file.path(data_dir, "01_coding_06_mean_01_sub.csv")
file_path_b <- file.path(data_dir, "01_coding_06_mean_02_total.csv")
df <- read_csv(file = file_path) %>% tibble()
df_a <- read_csv(file = file_path_a) %>% tibble()
df_b <- read_csv(file = file_path_b) %>% tibble()


#### Check variable types
head(df_a)
head(df_b)
head(df)


nrow(df)
nrow(df_a)
nrow(df_b)

df_na <- na.omit(df)
df_na_a <- na.omit(df_a)
df_na_b <- na.omit(df_b)

nrow(df_na)
nrow(df_na_a)
nrow(df_na_b)


nrow(df_na_aa)
nrow(df_na_bb)

names(df_a)
names(df_na_b)

### make variable
df_indepen_n <- df_na_b[,2:5]
df_indepen_w <- df_na_b[,6:9]
df_indepen_v <- df_na_b[,10:13]
df_medi <- df_na_b[,14:19]
df_depen_1 <- df_na_b[,20:23]
df_depen_2 <- df_na_b[,24:25]
df_depen_new <- df_na_b[,c(47,30,48)]

df_info <- df_na_b[c(26:38)]
table(df_info$Grade)
table(df_info$gender)
table(df_info$Depart)
table(df_info$Major)
table(df_info$Htype)
table(df_info$Hregion)
table(df_info$Residence)

cor(df_info)

cor.test(df_info$Htype, df_info$Major)
cor.test(df_info$Htype, df_info$Depart)
cor.test(df_info$Htype, df_info$Hregion)

cor.test(df_info$Depart, df_info$Major)
cor.test(df_info$Depart, df_info$gender)

cor.test(df_info$gender, df_info$Major)

cor.test(df_info$Hregion, df_info$Grade)
cor.test(df_info$Hregion, df_info$Residence)

cor.test(df_info$Residence, df_info$Grade)



###' ###########################################################################'
###' 
###' Import survey items
###' 
###' 

#### Check variable

lapply(df_na_a, class)
lapply(df_na_b, class)

df_indepen_n$a <- (df_indepen_n$R_total / 3)
map(df_indepen_n,summary)
lapply(df_indepen_n, sd)

df_medi$a <- (df_medi$Resili_total / 5)
map(df_medi,summary)
lapply(df_medi, sd)

summary(v)
sd(df_na_b$UnivEduSatis_total_a)

map(df_depen_2,summary)
lapply(df_depen_2, sd)

df_na_b$ac_1 <- (df_na_b$UnivEduSatis_total_a + df_na_b$GPA_new_5)/2
df_na_b$ac_2 <- (df_na_b$UnivEduSatis_total_a + df_na_b$GPA_scaled_new_5)/2
summary(df_na_b$ac_1)
sd(df_na_b$ac_1)
summary(df_na_b$ac_2)
sd(df_na_b$ac_2)
names(df_na_b)


###' ###########################################################################'
###' 
###' Normality test
###' 
###' 


### Personal Relationship
a <- ggplot(data = df_indepen_n) + geom_density(mapping=aes(x=R_friend))
b <- ggplot(data = df_indepen_n) + geom_density(mapping=aes(x=R_prof))
c <- ggplot(data = df_indepen_n) + geom_density(mapping=aes(x=R_parent))
d <- ggplot(data = df_indepen_n) + geom_density(mapping=aes(x=R_total))
grid.arrange(a,b,c,d, nrow=1, ncol=4)

skewness(df_indepen_n$R_friend)
kurtosis(df_indepen_n$R_friend)
shapiro.test(df_indepen_n$R_friend)
ks.test(df_indepen_n$R_friend, "pnorm")
qqnorm(df_indepen_n$R_parent)

skewness(df_indepen_n$R_prof)
kurtosis(df_indepen_n$R_prof)
shapiro.test(df_indepen_n$R_prof)
ks.test(df_indepen_n$R_prof, "pnorm")

skewness(df_indepen_n$R_parent)
kurtosis(df_indepen_n$R_parent)
shapiro.test(df_indepen_n$R_parent)
ks.test(df_indepen_n$R_parent, "pnorm")

skewness(df_indepen_n$a)
kurtosis(df_indepen_n$a)
shapiro.test(df_indepen_n$R_total)
ks.test(df_indepen_n$R_total, "pnorm", mean= mean(df_indepen_n$R_total), sd = sd(df_indepen_n$R_friend))




kurtosis(df_na_a$R_friend_famil)
kurtosis(df_na_a$R_friend_credit)
kurtosis(df_na_a$R_friend_respect)

kurtosis(df_na_a$R_prof_famil)
kurtosis(df_na_a$R_prof_credit)
kurtosis(df_na_a$R_prof_respect)

kurtosis(df_na_a$R_parent_famil)
kurtosis(df_na_a$R_parent_credit)
kurtosis(df_na_a$R_parent_respect)


### Resilience
a <- ggplot(data = df_medi) + geom_density(mapping=aes(x=Resili_SelfEffi))
b <- ggplot(data = df_medi) + geom_density(mapping=aes(x=Resili_SituJuge))
c <- ggplot(data = df_medi) + geom_density(mapping=aes(x=Resili_Resource))
d <- ggplot(data = df_medi) + geom_density(mapping=aes(x=Resili_Vital))
e <- ggplot(data = df_medi) + geom_density(mapping=aes(x=Resili_FutureOrien))
f <- ggplot(data = df_medi) + geom_density(mapping=aes(x=Resili_total))
grid.arrange(a,b,c,d,e,f, nrow=2, ncol=3)

skewness(df_medi$Resili_SelfEffi)
kurtosis(df_medi$Resili_SelfEffi)
shapiro.test(df_medi$Resili_SelfEffi)
ks.test(df_indepen_n$Resili_SelfEffi, "pnorm", mean= mean(df_medi$Resili_SelfEffi), sd = sd(df_medi$Resili_SelfEffi))

skewness(df_medi$Resili_SituJuge)
kurtosis(df_medi$Resili_SituJuge)
shapiro.test(df_medi$Resili_SituJuge)
ks.test(df_medi$Resili_SituJuge, "pnorm", mean= mean(df_medi$Resili_SituJuge), sd = sd(df_medi$Resili_SituJuge))

skewness(df_medi$Resili_Resource)
kurtosis(df_medi$Resili_Resource)
shapiro.test(df_medi$Resili_Resource)
ks.test(df_medi$Resili_Resource, "pnorm", mean= mean(df_medi$Resili_Resource), sd = sd(df_medi$Resili_Resource))

skewness(df_medi$Resili_Vital)
kurtosis(df_medi$Resili_Vital)
shapiro.test(df_medi$Resili_Vital)
ks.test(df_medi$Resili_Vital, "pnorm", mean= mean(df_medi$Resili_Vital), sd = sd(df_medi$Resili_Vital))

skewness(df_medi$Resili_FutureOrien)
kurtosis(df_medi$Resili_FutureOrien)
shapiro.test(df_medi$Resili_FutureOrien)
ks.test(df_medi$Resili_FutureOrien, "pnorm", mean= mean(df_medi$Resili_FutureOrien), sd = sd(df_medi$Resili_FutureOrien))

skewness(df_medi$a)
kurtosis(df_medi$a)
shapiro.test(df_medi$Resili_total)
ks.test(df_medi$Resili_total, "pnorm", mean= mean(df_medi$Resili_total), sd = sd(df_medi$Resili_total))



### UnivEduSatis
a <- ggplot(data = df_depen_1) + geom_density(mapping=aes(x=UnivEduSatis_liberalarts))
b <- ggplot(data = df_depen_1) + geom_density(mapping=aes(x=UnivEduSatis_major))
c <- ggplot(data = df_depen_1) + geom_density(mapping=aes(x=UnivEduSatis_infra))
d <- ggplot(data = df_depen_1) + geom_density(mapping=aes(x=UnivEduSatis_total))
grid.arrange(a,b,c,d, nrow=1, ncol=4)


skewness(df_depen_1$UnivEduSatis_liberalarts)
kurtosis(df_depen_1$UnivEduSatis_liberalarts)
shapiro.test(df_depen_1$UnivEduSatis_liberalarts)
ks.test(df_depen_1$UnivEduSatis_liberalarts, "pnorm")

skewness(df_depen_1$UnivEduSatis_major)
kurtosis(df_depen_1$UnivEduSatis_major)
shapiro.test(df_depen_1$UnivEduSatis_major)
ks.test(df_depen_1$UnivEduSatis_major, "pnorm")

skewness(df_depen_1$UnivEduSatis_infra)
kurtosis(df_depen_1$UnivEduSatis_infra)
shapiro.test(df_depen_1$UnivEduSatis_infra)
ks.test(df_depen_1$UnivEduSatis_infra, "pnorm")

skewness(df_na_b$UnivEduSatis_total_a)
kurtosis(df_na_b$UnivEduSatis_total_a)
shapiro.test(df_depen_1$UnivEduSatis_total)
ks.test(df_depen_1$UnivEduSatis_total, "pnorm")

names(df_na_a)
skewness(df_na_a$UnivEduSatis_liberalarts_contents)
skewness(df_na_a$UnivEduSatis_liberalarts_method)
skewness(df_na_a$UnivEduSatis_liberalarts_assess)
skewness(df_na_a$UnivEduSatis_liberalarts_prof)


skewness(df_na_a$UnivEduSatis_major_contents)
skewness(df_na_a$UnivEduSatis_major_method)
skewness(df_na_a$UnivEduSatis_major_assess)
skewness(df_na_a$UnivEduSatis_major_prof)

skewness(df_na_a$UnivEduSatis_infra_computerized)
skewness(df_na_a$UnivEduSatis_infra_support)
skewness(df_na_a$UnivEduSatis_infra_facility)
skewness(df_na_a$UnivEduSatis_infra_program)




### GPA
a <- ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA))
b <- ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA_new))
c <- ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA_5))
grid.arrange(a,b,c, nrow=1, ncol=3)

skewness(df_na_b$ac_1)
kurtosis(df_na_b$ac_1)
shapiro.test(df_depen_2$GPA)
ks.test(df_depen_2$GPA, "pnorm", mean= mean(df_depen_2$GPA), sd = sd(df_depen_2$GPA))

summary(df_depen_2$GPA_new)
#for negatively skewed data
#df_depen_2$GPA_a <- sqrt(max(df_depen_2$GPA+1) - df_depen_2$GPA)
#df_depen_2$GPA_b <- log10(max(df_depen_2$GPA+1) - df_depen_2$GPA)
a <-  (df_depen_2$GPA - min(df_depen_2$GPA))/(max(df_depen_2$GPA) - min(df_depen_2$GPA))
ggplot(data = a) + geom_density(mapping=aes(x=a))

df_depen_2$GPA_new <- 1/(max(df_depen_2$GPA+1) - df_depen_2$GPA)
df_na_a$GPA_new <- 1/(max(df_depen_2$GPA+1) - df_depen_2$GPA)
df_na_b$GPA_new <- 1/(max(df_depen_2$GPA+1) - df_depen_2$GPA)

df_depen_2$GPA_scaled_new <- 1/(max(df_depen_2$GPA_scaled+1) - df_depen_2$GPA_scaled)
df_na_a$GPA_scaled_new <- 1/(max(df_depen_2$GPA_scaled+1) - df_depen_2$GPA_scaled)
df_na_b$GPA_scaled_new <- 1/(max(df_depen_2$GPA_scaled+1) - df_depen_2$GPA_scaled)

df_depen_2$GPA_new_5 <- df_depen_2$GPA_new*5
df_na_a$GPA_new_5 <- df_na_a$GPA_new*5
df_na_b$GPA_new_5 <- df_na_b$GPA_new*5

df_depen_2$GPA_scaled_new_5 <- df_depen_2$GPA_scaled_new*5
df_na_a$GPA_scaled_new_5 <- df_na_a$GPA_scaled_new*5
df_na_b$GPA_scaled_new_5 <- df_na_b$GPA_scaled_new*5


summary(df_depen_2$GPA)
summary(df_depen_2$GPA_new)
summary(df_depen_2$GPA_scaled_new)
summary(df_depen_2$GPA_new_5)
summary(df_depen_2$GPA_scaled_new_5)


df_na_a <- df_na_a %>%
  relocate(matches("^GPA"), .after = GPA)
df_na_b <- df_na_b %>%
  relocate(matches("^GPA"), .after = GPA)


ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA_a))
ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA_b))
b <- ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA_new_5))
c <- ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA_scaled_new_5))



grid.arrange(a,b,c, nrow=1, ncol=4)



###' ###########################################################################'
###' 
###' correlation
###' 


### Personal Relationship
 cor(df_indepen_n)
cor.test(df_indepen_n$R_total, df_indepen_n$R_friend)
cor.test(df_indepen_n$R_total, df_indepen_n$R_prof)
cor.test(df_indepen_n$R_total, df_indepen_n$R_parent)
cor.test(df_indepen_n$R_friend, df_indepen_n$R_prof)
cor.test(df_indepen_n$R_friend, df_indepen_n$R_parent)
cor.test(df_indepen_n$R_prof, df_indepen_n$R_parent)



### Resilience
cor(df_medi)
cor.test(df_medi$Resili_total, df_medi$Resili_SelfEffi)
cor.test(df_medi$Resili_total, df_medi$Resili_SituJuge)
cor.test(df_medi$Resili_total, df_medi$Resili_Resource)
cor.test(df_medi$Resili_total, df_medi$Resili_Vital)
cor.test(df_medi$Resili_total, df_medi$Resili_FutureOrien)

cor.test(df_medi$Resili_SelfEffi, df_medi$Resili_SituJuge)
cor.test(df_medi$Resili_SelfEffi, df_medi$Resili_Resource)
cor.test(df_medi$Resili_SelfEffi, df_medi$Resili_Vital)
cor.test(df_medi$Resili_SelfEffi, df_medi$Resili_FutureOrien)

cor.test(df_medi$Resili_SituJuge, df_medi$Resili_Resource)
cor.test(df_medi$Resili_SituJuge, df_medi$Resili_Vital)
cor.test(df_medi$Resili_SituJuge, df_medi$Resili_FutureOrien)

cor.test(df_medi$Resili_Resource, df_medi$Resili_Vital)
cor.test(df_medi$Resili_Resource, df_medi$Resili_FutureOrien)

cor.test(df_medi$Resili_Vital, df_medi$Resili_FutureOrien)



### UnivEduSatis
cor(df_depen_1)
cor.test(df_depen_1$UnivEduSatis_total, df_depen_1$UnivEduSatis_liberalarts)
cor.test(df_depen_1$UnivEduSatis_total, df_depen_1$UnivEduSatis_major)
cor.test(df_depen_1$UnivEduSatis_total, df_depen_1$UnivEduSatis_infra)
cor.test(df_depen_1$UnivEduSatis_liberalarts, df_depen_1$UnivEduSatis_major)
cor.test(df_depen_1$UnivEduSatis_liberalarts, df_depen_1$UnivEduSatis_infra)
cor.test(df_depen_1$UnivEduSatis_major, df_depen_1$UnivEduSatis_infra)



### ALL

cor(df_depen_new, df_indepen_n)
cor(df_medi, df_indepen_n)
cor(df_medi, df_depen_new)


### cor test indepen-medi
cor.test(df_indepen_n$R_friend, df_medi$Resili_SelfEffi)
cor.test(df_indepen_n$R_friend, df_medi$Resili_SituJuge)
cor.test(df_indepen_n$R_friend, df_medi$Resili_Resource)
cor.test(df_indepen_n$R_friend, df_medi$Resili_Vital)
cor.test(df_indepen_n$R_friend, df_medi$Resili_FutureOrien)
cor.test(df_indepen_n$R_friend, df_medi$a)

cor.test(df_indepen_n$R_prof, df_medi$Resili_SelfEffi)
cor.test(df_indepen_n$R_prof, df_medi$Resili_SituJuge)
cor.test(df_indepen_n$R_prof, df_medi$Resili_Resource)
cor.test(df_indepen_n$R_prof, df_medi$Resili_Vital)
cor.test(df_indepen_n$R_prof, df_medi$Resili_FutureOrien)
cor.test(df_indepen_n$R_prof, df_medi$a)

cor.test(df_indepen_n$R_parent, df_medi$Resili_SelfEffi)
cor.test(df_indepen_n$R_parent, df_medi$Resili_SituJuge)
cor.test(df_indepen_n$R_parent, df_medi$Resili_Resource)
cor.test(df_indepen_n$R_parent, df_medi$Resili_Vital)
cor.test(df_indepen_n$R_parent, df_medi$Resili_FutureOrien)
cor.test(df_indepen_n$R_parent, df_medi$a)

cor.test(df_indepen_n$a, df_medi$Resili_SelfEffi)
cor.test(df_indepen_n$a, df_medi$Resili_SituJuge)
cor.test(df_indepen_n$a, df_medi$Resili_Resource)
cor.test(df_indepen_n$a, df_medi$Resili_Vital)
cor.test(df_indepen_n$a, df_medi$Resili_FutureOrien)
cor.test(df_indepen_n$a, df_medi$a)



### cor test indepen-depen
cor.test(df_depen_new$UnivEduSatis_total_a, df_indepen_n$R_friend)
cor.test(df_depen_new$UnivEduSatis_total_a, df_indepen_n$R_prof)
cor.test(df_depen_new$UnivEduSatis_total_a, df_indepen_n$R_parent)
cor.test(df_depen_new$UnivEduSatis_total_a, df_indepen_n$a)

cor.test(df_depen_new$GPA_new_5, df_indepen_n$R_friend)
cor.test(df_depen_new$GPA_new_5, df_indepen_n$R_prof)
cor.test(df_depen_new$GPA_new_5, df_indepen_n$R_parent)
cor.test(df_depen_new$GPA_new_5, df_indepen_n$a)

cor.test(df_depen_new$ac_1, df_indepen_n$R_friend)
cor.test(df_depen_new$ac_1, df_indepen_n$R_prof)
cor.test(df_depen_new$ac_1, df_indepen_n$R_parent)
cor.test(df_depen_new$ac_1, df_indepen_n$a)


### cor test medi-depen
cor.test(df_depen_new$UnivEduSatis_total_a, df_medi$Resili_SelfEffi)
cor.test(df_depen_new$UnivEduSatis_total_a, df_medi$Resili_SituJuge)
cor.test(df_depen_new$UnivEduSatis_total_a, df_medi$Resili_Resource)
cor.test(df_depen_new$UnivEduSatis_total_a, df_medi$Resili_Vital)
cor.test(df_depen_new$UnivEduSatis_total_a, df_medi$Resili_FutureOrien)
cor.test(df_depen_new$UnivEduSatis_total_a, df_medi$a)

cor.test(df_depen_new$GPA_new_5, df_medi$Resili_SelfEffi)
cor.test(df_depen_new$GPA_new_5, df_medi$Resili_SituJuge)
cor.test(df_depen_new$GPA_new_5, df_medi$Resili_Resource)
cor.test(df_depen_new$GPA_new_5, df_medi$Resili_Vital)
cor.test(df_depen_new$GPA_new_5, df_medi$Resili_FutureOrien)
cor.test(df_depen_new$GPA_new_5, df_medi$a)

cor.test(df_depen_new$ac_1, df_medi$Resili_SelfEffi)
cor.test(df_depen_new$ac_1, df_medi$Resili_SituJuge)
cor.test(df_depen_new$ac_1, df_medi$Resili_Resource)
cor.test(df_depen_new$ac_1, df_medi$Resili_Vital)
cor.test(df_depen_new$ac_1, df_medi$Resili_FutureOrien)
cor.test(df_depen_new$ac_1, df_medi$a)



###' ###########################################################################'
###' 
###' CFA, cronbach's alpha
###' 
###'


df_cron_f <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/GraduationThesis/datasets/01_coding_05_full_1234_A.csv")
df_cron <- na.omit(df_cron_f)
nrow(df_cron)
names(df_cron)


### Personal Relationship
cronbach(df_cron[,3:11])
cronbach(df_cron[,12:20])
cronbach(df_cron[,21:29])
cronbach(df_cron[,3:29])

cronbach(df_na_a[,2:4])
cronbach(df_na_a[,5:7])
cronbach(df_na_a[,8:10])
cronbach(df_na_a[,2:10])

alpha(df_na_b[,2:4])
cronbach(df_na_b[,2:5])


cfa_indepen <-
  'R_total =~ R_friend + R_prof + R_parent
  R_friend =~ R_friend_famil + R_friend_credit + R_friend_respect
  R_prof =~ NA*R_prof_famil + 1*R_prof_credit + R_prof_respect
  R_parent =~ R_parent_famil + R_parent_credit + R_parent_respect'


cfa_indepen_out <- cfa(cfa_indepen, df_na_a)
fitMeasures(cfa_indepen_out, c('chisq','df','pvalue','tli','cfi','rmsea','rmsea.ci.lower','rmsea.ci.upper','srmr'))
summary(cfa_indepen_out, fit.measures=T, standardized=T)
summary(cfa_indepen_out, fit.measures=T, standardized=T, rsquare=T)



#reliability
ROK_R <- reliability(cfa_indepen_out, return.total = TRUE)


# table
standardizedsolution(cfa_indepen_out) %>%
  filter(op=="=~") %>%
  mutate(stars = ifelse(pvalue<0.001, "***", ifelse(pvalue<0.01, "**", ifelse(pvalue<0.05, "*", ""))))


resid.cor <- residuals(cfa_indepen_out, type="cor")$cov
resid.cor[upper.tri(resid.cor, diag=TRUE)] <- NA
kable(resid.cor, digits=2, format="pandoc", caption="Residual Correlations")

#graphic_ROKfit
diagra_ROK <- semPlot::semPaths(cfa_indepen_out, whatLabels = "std", intercepts = FALSE,
                                style = "lisrel",
                                nCharNodes = 0,
                                nCharEdges = 0,
                                edge.color = "black",
                                curveAdjacent = TRUE, title = TRUE,
                                layout = "tree", curvePivot = TRUE)





### Academic Resilience

names(df_cron)
cronbach(df_cron[,44:52])
cronbach(df_cron[,53:61])
cronbach(df_cron[,62:70])
cronbach(df_cron[,71:79])
cronbach(df_cron[,80:88])
cronbach(df_cron[,44:88])

cronbach(df_na_a[,57:59])
cronbach(df_na_a[,60:62])
cronbach(df_na_a[,63:65])
cronbach(df_na_a[,66:68])
cronbach(df_na_a[,69:71])
cronbach(df_na_a[,57:71])

cronbach(df_na_a[,29:31])
cronbach(df_na_a[,32:34])
cronbach(df_na_a[,35:37])
cronbach(df_na_a[,38:40])
cronbach(df_na_a[,41:43])
cronbach(df_na_a[,29:43])

alpha(df_na_b[,14:18])
cronbach(df_na_b[,14:18])


cfa_medi <-
  'Resili_total =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  Resili_SelfEffi =~ Resili_SelfEffi_S1 + Resili_SelfEffi_S2 + Resili_SelfEffi_S3
  Resili_SituJuge =~ Resili_SituJuge_S1 + Resili_SituJuge_S2 + Resili_SituJuge_S3
  Resili_Resource =~ Resili_Resource_S1 + Resili_Resource_S2 + Resili_Resource_S3
  Resili_Vital =~ Resili_Vital_S1 + Resili_Vital_S2 + Resili_Vital_S3
  Resili_FutureOrien =~ Resili_FutureOrien_S1 + Resili_FutureOrien_S2 +Resili_FutureOrien_S3'


cfa_medi_out <- cfa(cfa_medi, df_na_a)
fitMeasures(cfa_medi_out, c('chisq','df','pvalue','tli','cfi','rmsea','rmsea.ci.lower','rmsea.ci.upper','srmr'))
summary(cfa_medi_out, fit.measures=T, standardized=T)
summary(cfa_medi_out, fit.measures=T, standardized=T, rsquare=T)
lavInspect(cfa_medi_out,"cov.lv")


#reliability
ROK_R <- reliability(cfa_medi_out, return.total = TRUE)


### Univ. Education Satisfaction
cronbach(df_cron[,91:94])
cronbach(df_cron[,95:98])
cronbach(df_cron[,99:102])
cronbach(df_cron[,91:102])

cronbach(df_na_a[,44:47])
cronbach(df_na_a[,48:51])
cronbach(df_na_a[,52:55])
cronbach(df_na_a[,44:55])


alpha(df_na_b[,20:22])
cronbach(df_na_b[,20:22])



cfa_depen <-
  'UnivEduSatis_total =~  UnivEduSatis_liberalarts + UnivEduSatis_major + UnivEduSatis_infra
   UnivEduSatis_liberalarts =~ NA*UnivEduSatis_liberalarts_contents + 1*UnivEduSatis_liberalarts_method + UnivEduSatis_liberalarts_assess + UnivEduSatis_liberalarts_prof
   UnivEduSatis_major =~ NA*UnivEduSatis_major_contents + 1*UnivEduSatis_major_method + UnivEduSatis_major_assess + UnivEduSatis_major_prof
   UnivEduSatis_infra =~ UnivEduSatis_infra_computerized + UnivEduSatis_infra_support + UnivEduSatis_infra_facility + UnivEduSatis_infra_program'


cfa_depen_out <- cfa(cfa_depen, df_na_a)
fitMeasures(cfa_depen_out, c('chisq','df','pvalue','tli','cfi','rmsea','rmsea.ci.lower','rmsea.ci.upper','srmr'))
summary(cfa_depen_out, fit.measures=T, standardized=T)
summary(cfa_depen_out, fit.measures=T, standardized=T, rsquare=T)
lavInspect(cfa_depen_out,"cov.lv")
#corr.test(a)


#reliability
ROK_R <- reliability(cfa_depen_out, return.total = TRUE)


# table
standardizedsolution(cfa_depen_out) %>%
  filter(op=="=~") %>%
  mutate(stars = ifelse(pvalue<0.001, "***", ifelse(pvalue<0.01, "**", ifelse(pvalue<0.05, "*", ""))))


resid.ROKfit <- residuals(cfa_depen_out, type="cor")$cov
resid.ROKfit[upper.tri(resid.cor, diag=TRUE)] <- NA
kable(resid.ROKfit, digits=2, format="pandoc", caption="Residual Correlations")

#graphic
semPlot::semPaths(cfa_depen_out, whatLabels = "std", intercepts = FALSE,
                  style = "lisrel",
                  nCharNodes = 0,
                  nCharEdges = 0,
                  edge.color = "black",
                  curveAdjacent = TRUE, title = TRUE,
                  layout = "tree", curvePivot = TRUE)




###' ###########################################################################'
###' 
###' SEM: 1
###' 
###'


names(df_na_b)

### only UnivSatis
sem_model_1 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivSatis =~ UnivEduSatis_liberalarts + UnivEduSatis_major + UnivEduSatis_infra
  UnivSatis ~ PersonalR + AcademicR
  AcademicR ~ PersonalR'

out_1 <- sem(sem_model_1, df_na_b)
fitMeasures(out_1 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr")) %>%
  kable (format="pandoc",caption = "Model fit information")
summary(out_1, fit.measures=T, standardized=T, rsquare=T)

#graphic
semPlot::semPaths(out_1, whatLabels = "std", intercepts = FALSE,
                  style = "lisrel",
                  nCharNodes = 0,
                  nCharEdges = 0,
                  edge.color = "black",
                  
                  curveAdjacent = TRUE, title = TRUE,
                  layout = "tree", curvePivot = FALSE)


### only GPA
sem_model_2_1 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
 
  GPA ~ PersonalR + AcademicR
  AcademicR ~ PersonalR'

out_2_1 <- sem(sem_model_2_1, df_na_b)
fitMeasures(out_2_1 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr")) %>%
  kable (format="pandoc",caption = "Model fit information")
summary(out_2_1, fit.measures=T, standardized=T, rsquare=T)



### only GPA_norm
sem_model_2_2 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
 
  GPA_new ~ PersonalR + AcademicR
  AcademicR ~ PersonalR'

out_2_2 <- sem(sem_model_2_2, df_na_b)
fitMeasures(out_2_2 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr")) %>%
  kable (format="pandoc",caption = "Model fit information")
summary(out_2_2, fit.measures=T, standardized=T, rsquare=T)



### only GPA_scaled_norm
sem_model_2_3 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
 
  GPA_5_scaled ~ PersonalR + AcademicR
  AcademicR ~ PersonalR'

out_2_3 <- sem(sem_model_2_3, df_na_b)
fitMeasures(out_2_3 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr")) %>%
  kable (format="pandoc",caption = "Model fit information")
summary(out_2_3, fit.measures=T, standardized=T, rsquare=T)

#graphic
semPlot::semPaths(out_2_3, whatLabels = "std", intercepts = FALSE,
                  style = "lisrel",
                  nCharNodes = 0,
                  nCharEdges = 0,
                  edge.color = "black",
                  curveAdjacent = FALSE, title = TRUE,
                  layout = "tree", curvePivot = FALSE)



###' ###########################################################################'
###' 
###' SEM: 2
###' 
###'


### UnivSatis, GPA repective
sem_model_3 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivSatis =~ UnivEduSatis_liberalarts + UnivEduSatis_major + UnivEduSatis_infra
  
  UnivSatis ~ PersonalR + AcademicR
  GPA ~ PersonalR + AcademicR
  AcademicR ~ PersonalR'

out_3 <- sem(sem_model_3, df_na_b)
fitMeasures(out_3 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr")) %>%
  kable (format="pandoc",caption = "Model fit information")
summary(out_3, fit.measures=T, standardized=T, rsquare=T)

#graphic
semPlot::semPaths(out_3, whatLabels = "std", intercepts = FALSE,
                  style = "lisrel",
                  nCharNodes = 0,
                  nCharEdges = 0,
                  edge.color = "black",
                  curveAdjacent = FALSE, title = TRUE,
                  layout = "tree", curvePivot = FALSE)


### UnivSatis, GPA_scaled_norm repective
sem_model_3_1 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivSatis =~ UnivEduSatis_liberalarts + UnivEduSatis_major + UnivEduSatis_infra
  
  UnivSatis ~ PersonalR + AcademicR
  GPA_5 ~ PersonalR + AcademicR
  AcademicR ~ PersonalR'

out_3_1 <- sem(sem_model_3_1, df_na_b)
fitMeasures(out_3_1 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr"))
summary(out_3_1, fit.measures=T, standardized=T, rsquare=T)



### UnivSatis, GPA_norm together_1st
sem_model_4_1 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivSatis =~ UnivEduSatis_liberalarts + UnivEduSatis_major + UnivEduSatis_infra
  UnivEduAchive =~ UnivSatis + GPA
  
  UnivEduAchive ~ PersonalR + AcademicR
  AcademicR ~ PersonalR'

out_4_1 <- sem(sem_model_4_1, df_na_b)
fitMeasures(out_4_1 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr")) %>%
  kable (format="pandoc",caption = "Model fit information")
summary(out_4_1, fit.measures=T, standardized=T, rsquare=T)


sem_model_4_2 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivSatis =~ UnivEduSatis_liberalarts + UnivEduSatis_major + UnivEduSatis_infra
  UnivEduAchive =~ UnivSatis + GPA_5_scaled
  UnivEduAchive ~ PersonalR + AcademicR
  AcademicR ~ PersonalR'

out_4_2 <- sem(sem_model_4_2, df_na_b)
fitMeasures(out_4_2 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr"))
summary(out_4_2, fit.measures=T, standardized=T, rsquare=T)



df_na_b$UnivEduSatis_total_a <- df_na_b$UnivEduSatis_total/3
### UnivSatis, GPA_norm together_ 2nd
sem_model_5_1 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA
  UnivEduAchive ~ PersonalR + AcademicR
  AcademicR ~ PersonalR'

out_5_1 <- sem(sem_model_5_1, df_na_b)
fitMeasures(out_5_1 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr"))
summary(out_5_1, fit.measures=T, standardized=T, rsquare=T)

summary(df_na_b$GPA_new_5)
#####################################################################################
#####################################################################################
sem_model_5_2 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ NA*Resili_SelfEffi + Resili_SituJuge + 1*Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_new_5
  
UnivEduAchive ~ PersonalR + AcademicR
AcademicR ~ PersonalR'



sem_model_5_2 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ NA*Resili_SelfEffi + Resili_SituJuge + 1*Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_new_5

PersonalR ~ C1*GradeF + C2*HregionF
UnivEduAchive ~ PersonalR + AcademicR +  C3*HregionF + C4*GradeF
AcademicR ~ PersonalR + C5*HregionF + C6*GradeF'


table(df_na_b$Hregion_O)
table(df_na_b$HregionF)

df_na_b$HregionF <- ifelse(df_na_b$Hregion_O == 1, "big",
                         ifelse(df_na_b$Hregion_O == 2 | df_na_b$Hregion_O == 3, "middle",
                                ifelse(df_na_b$Hregion_O == 4, "small",NA)))
df_na_b <-transform(df_na_b,
          seoul = ifelse(Hregion==1, 1, 0),
          middle = ifelse(Hregion==2, 1, 0),
          small = ifelse(Hregion==3, 1, 0),
          other = ifelse(Hregion==4, 1, 0))

names(df_na_b)
df_na_b$GradeF <- as.factor(df_na_b$Grade)
df_na_b$genderF <- as.factor(df_na_b$gender)
df_na_b$HregionF <- as.factor(df_na_b$HregionF)


out_5_2 <- sem(sem_model_5_2, df_na_b)
fitMeasures(out_5_2 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr"))
summary(out_5_2, fit.measures=T, standardized=T, rsquare=T)


#graphic
semPlot::semPaths(out_5_2, whatLabels = "std", intercepts = FALSE,
                  style = "lisrel",
                  nCharNodes = 0,
                  nCharEdges = 0,
                  edge.color = "black",
                  curveAdjacent = FALSE, title = TRUE,
                  layout = "tree", curvePivot = FALSE)



###' ###########################################################################'
###' 
###' with items (df_na_b)
###' 
###'


### UnivSatis, GPA_norm together_ 3rd
sem_model_6_1 <-
  'R_friend =~ R_friend_famil + R_friend_credit + R_friend_respect
  R_prof =~ R_prof_famil + R_prof_credit + R_prof_respect
  R_parent =~ R_parent_famil + R_parent_credit + R_parent_respect
  
  Resili_SelfEffi =~ Resili_SelfEffi_S1 + Resili_SelfEffi_S2 + Resili_SelfEffi_S3
  Resili_SituJuge =~ Resili_SituJuge_S1 + Resili_SituJuge_S2 + Resili_SituJuge_S3
  Resili_Resource =~ Resili_Resource_S1 + Resili_Resource_S2 + Resili_Resource_S3
  Resili_Vital =~ Resili_Vital_S1 + Resili_Vital_S2 + Resili_Vital_S3
  Resili_FutureOrien =~ Resili_FutureOrien_S1 + Resili_FutureOrien_S2 + Resili_FutureOrien_S3
  
  UnivEduSatis_liberalart =~ UnivEduSatis_liberalarts_contents + UnivEduSatis_liberalarts_method + UnivEduSatis_liberalarts_assess + UnivEduSatis_liberalarts_prof
  UnivEduSatis_major =~ UnivEduSatis_major_contents + UnivEduSatis_major_method + UnivEduSatis_major_assess + UnivEduSatis_major_prof
  UnivEduSatis_infra =~ UnivEduSatis_infra_computerized + UnivEduSatis_infra_support + UnivEduSatis_infra_facility + UnivEduSatis_infra_program
  UnivEduSatis =~ UnivEduSatis_liberalart + UnivEduSatis_major + UnivEduSatis_infra
  
  PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis + GPA
  
  UnivEduAchive ~ PersonalR + AcademicR
  AcademicR ~ PersonalR'


out_6_1 <- sem(sem_model_6_1, df_na_a)
fitMeasures(out_6_1 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr")) %>%
  kable (format="pandoc",caption = "Model fit information")
summary(out_6_1, fit.measures=T, standardized=T, rsquare=T)



sem_model_6_2 <-
  'R_friend =~ R_friend_famil + R_friend_credit + R_friend_respect
  R_prof =~ R_prof_famil + R_prof_credit + R_prof_respect
  R_parent =~ R_parent_famil + R_parent_credit + R_parent_respect
  
  Resili_SelfEffi =~ Resili_SelfEffi_S1 + Resili_SelfEffi_S2 + Resili_SelfEffi_S3
  Resili_SituJuge =~ Resili_SituJuge_S1 + Resili_SituJuge_S2 + Resili_SituJuge_S3
  Resili_Resource =~ Resili_Resource_S1 + Resili_Resource_S2 + Resili_Resource_S3
  Resili_Vital =~ Resili_Vital_S1 + Resili_Vital_S2 + Resili_Vital_S3
  Resili_FutureOrien =~ Resili_FutureOrien_S1 + Resili_FutureOrien_S2 + Resili_FutureOrien_S3
  
  UnivEduSatis_liberalart =~ UnivEduSatis_liberalarts_contents + UnivEduSatis_liberalarts_method + UnivEduSatis_liberalarts_assess + UnivEduSatis_liberalarts_prof
  UnivEduSatis_major =~ UnivEduSatis_major_contents + UnivEduSatis_major_method + UnivEduSatis_major_assess + UnivEduSatis_major_prof
  UnivEduSatis_infra =~ UnivEduSatis_infra_computerized + UnivEduSatis_infra_support + UnivEduSatis_infra_facility + UnivEduSatis_infra_program
  UnivEduSatis =~ UnivEduSatis_liberalart + UnivEduSatis_major + UnivEduSatis_infra
  
  PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis + GPA_5_scaled
  
  UnivEduAchive ~ PersonalR + AcademicR
  AcademicR ~ PersonalR

  R_friend ~~ R_prof
  R_prof ~~ R_parent'


out_6_2 <- sem(sem_model_6_2, df_na_a)
fitMeasures(out_6_2 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr"))
summary(out_6_2, fit.measures=T, standardized=T, rsquare=T)




###' ###########################################################################'
###' 
###' Mediation Effect Analysis
###' 
###'


### Mediation Effect Analysis_1
sem.med_1 <- 
  # measurement model
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ NA*Resili_SelfEffi + Resili_SituJuge + 1*Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_new_5
  
  # regressions
  AcademicR ~ a*PersonalR
  UnivEduAchive ~ c*PersonalR + b*AcademicR

  # indirect effect: ab
  indirect := a*b
  # total effect: d
  direct := c
  total := c + (a*b)'


set.seed(123)
fit.med_1 <- sem(model=sem.med_1, data=df_na_b, se="bootstrap", bootstrap=1000)
fitMeasures(fit.med_1 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr"))
parameterestimates(fit.med_1)
summary(fit.med_1, standardized=TRUE)



a = 0.614 ; b = 0.250
s_a = 0.161 ;  s_b = 0.092
z = a * b / sqrt(b^2 * s_a^2 + a^2 * s_b^2)
z

2 * pnorm(z, lower.tail = F)



#####################################################################################
#####################################################################################
### Mediation Effect Analysis_2
sem.med_2 <- 
  # measurement model
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ NA*Resili_SelfEffi + Resili_SituJuge + 1*Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_new_5
  
  # regressions
PersonalR ~ C1*GradeF + C2*HregionF
UnivEduAchive ~ c*PersonalR + b*AcademicR +  C3*HregionF + C4*GradeF
AcademicR ~ a*PersonalR + C5*HregionF + C6*GradeF


  # indirect effect: ab
  indirect := a*b
  # total effect: d
  direct := c
  total := c + (a*b)'


set.seed(123)
fit.med_2 <- sem(model=sem.med_2, data=df_na_b, se="bootstrap", bootstrap=1000)

fitMeasures(fit.med_2 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr"))
summary(fit.med_2, standardized=TRUE)
parameterestimates(fit.med_2)

a = 0.579 ; b =   0.269  
s_a = 0.161     ;  s_b =  0.097
z = a * b / sqrt(b^2 * s_a^2 + a^2 * s_b^2)
z

2 * pnorm(z, lower.tail = F)


table(df_na_b$Grade)



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
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_new_5
              
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
fitMeasures(fit.mod ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr"))
parameterEstimates(fit.mod, standardized=TRUE)
summary(fit.mod, standardized=TRUE)



###' ###########################################################################'
###' 
###' Moderated Mediation Effect Analysis
###' 
###'




###' ###########################################################################'
###' 
###' Moderated Mediation Effect Analysis
###' 
###'



set.seed(123)
df_na_b.mod.med_1 <- indProd(df_na_b, var1=c("R_friend", "R_prof", "R_parent", "Resili_SelfEffi", "Resili_SituJuge", "Resili_Resource", "Resili_Vital", "Resili_FutureOrien"),
                             var2="gender", match=FALSE,
                             meanC=TRUE, residualC=FALSE, doubleMC=TRUE)


mod.model_1 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_new_5
  
  Personalgender =~ R_friend.gender + R_prof.gender + R_parent.gender
    Academicgender =~ Resili_SelfEffi.gender + Resili_SituJuge.gender + Resili_Resource.gender + Resili_Vital.gender + Resili_FutureOrien.gender
   
  AcademicR ~ a1*PersonalR + a2*gender + a3*Personalgender 
  UnivEduAchive ~ c1*PersonalR + b1*AcademicR 
  
  Resili_SelfEffi.gender ~~    Resili_SituJuge.gender
  Resili_Resource.gender ~~       Resili_Vital.gender
  Resili_SituJuge.gender ~~ Resili_FutureOrien.gender
  Resili_SelfEffi.gender ~~       Resili_Vital.gender
  Resili_SituJuge.gender ~~    Resili_Resource.gender
  Resili_Resource.gender ~~ Resili_FutureOrien.gender

  R_prof.gender ~~    Resili_SelfEffi.gender
  R_prof.gender ~~       Resili_Vital.gender
  R_prof.gender ~~    Resili_Resource.gender 
  R_prof.gender ~~           R_parent.gender
  Resili_Resource ~~              Resili_Vital
  


  first_F := a1 + a3*1
  first_M := a1 + a3*2
  
  middle_F := b1
  middle_M := b1
  
  indirect_F := first_F*b1
  indirect_M := first_M*b1
  
  direct_F := c1 
  direct_M := c1 
  
  
  total_F := indirect_F + direct_F
  total_M := indirect_M + direct_M'

set.seed(123)
fit.mod.med_1 <- sem(model=mod.model_1, data=df_na_b.mod.med_1)
fitMeasures(fit.mod.med_1 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr"))
summary(fit.mod.med_1, standardized=TRUE)
a$Modification Indices(sort=TRUE)

modindices(fit.mod.med_1, sort. = TRUE)





set.seed(123)
df_na_b.mod.med_2 <- indProd(df_na_b, var1=c("R_friend", "R_prof", "R_parent", "Resili_SelfEffi", "Resili_SituJuge", "Resili_Resource", "Resili_Vital", "Resili_FutureOrien"),
                             var2="gender", match=FALSE,
                             meanC=TRUE, residualC=FALSE, doubleMC=TRUE)


mod.model_2 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_new_5
  
  Personalgender =~ R_friend.gender + R_prof.gender + R_parent.gender
  Academicgender =~ Resili_SelfEffi.gender + Resili_SituJuge.gender + Resili_Resource.gender + Resili_Vital.gender + Resili_FutureOrien.gender
   
  AcademicR ~ a1*PersonalR
  UnivEduAchive ~ c1*PersonalR + b1*AcademicR + b2*gender + b3*Academicgender
  
  Resili_SelfEffi.gender ~~    Resili_SituJuge.gender
  Resili_Resource.gender ~~       Resili_Vital.gender
  Resili_SituJuge.gender ~~ Resili_FutureOrien.gender
  Resili_SelfEffi.gender ~~       Resili_Vital.gender
  Resili_SituJuge.gender ~~    Resili_Resource.gender
  Resili_Resource.gender ~~ Resili_FutureOrien.gender

  R_prof.gender ~~    Resili_SelfEffi.gender
  R_prof.gender ~~       Resili_Vital.gender
  R_prof.gender ~~    Resili_Resource.gender 
  R_prof.gender ~~           R_parent.gender
  Resili_Resource ~~              Resili_Vital

  first_F := a1
  first_M := a1 
  
  middle_F := b1 + b3*1
  middle_M := b1 + b3*2
  
  indirect_F := first_F*middle_F
  indirect_M := first_M*middle_M
  
  direct_F := c1 
  direct_M := c1 
  
  
  total_F := indirect_F + direct_F
  total_M := indirect_M + direct_M'

set.seed(123)
fit.mod.med_2 <- sem(model=mod.model_2, data=df_na_b.mod.med_2)
fitMeasures(fit.mod.med_2 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr"))
summary(fit.mod.med_2, standardized=TRUE)






set.seed(123)
df_na_b.mod.med_3 <- indProd(df_na_b, var1=c("R_friend", "R_prof", "R_parent", "Resili_SelfEffi", "Resili_SituJuge", "Resili_Resource", "Resili_Vital", "Resili_FutureOrien"),
                             var2="gender", match=FALSE,
                             meanC=TRUE, residualC=FALSE, doubleMC=TRUE)


mod.model_3 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_new_5
  
  Personalgender =~ R_friend.gender + R_prof.gender + R_parent.gender
  Academicgender =~ Resili_SelfEffi.gender + Resili_SituJuge.gender + Resili_Resource.gender + Resili_Vital.gender + Resili_FutureOrien.gender
   
  AcademicR ~ a1*PersonalR + a2*gender + a3*Personalgender 
  UnivEduAchive ~ c1*PersonalR + b1*AcademicR + b2*gender + b3*Academicgender
  
  
  first_F := a1 + a3*1
  first_M := a1 + a3*2 
  
  middle_F := b1 + b3*1
  middle_M := b1 + b3*2
  
  indirect_F := first_F*middle_F
  indirect_M := first_M*middle_M
  
  med.mod := indirect_M - indirect_F
  
  direct_F := c1 
  direct_M := c1 
  
  
  total_F := indirect_F + direct_F
  total_M := indirect_M + direct_M'

set.seed(123)
fit.mod.med_3 <- sem(model=mod.model_3, data=df_na_b.mod.med_3)
fitMeasures(fit.mod.med_3 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr"))
summary(fit.mod.med_3, standardized=TRUE)







table(df_na_b$gender)


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
<<<<<<< HEAD
PersonalR ~ C1*GradeF + C2*HregionF
UnivEduAchive ~ c*PersonalR + b*AcademicR +  C3*HregionF + C4*GradeF
AcademicR ~ a*PersonalR + C5*HregionF + C6*GradeF'
=======
  PersonalR ~ C1*Grade + C4*Hregion
  AcademicR ~ a*PersonalR + C2*Grade + C5*Hregion
  UnivEduAchive ~ c*PersonalR + b*AcademicR +  C3*Grade + C5*Hregion'
>>>>>>> edac05ab7620e409e4c8943fe3b171a8b1a39c12


sen_model <- 
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_new_5
    
  PersonalR ~ phantom1*phantom
  AcademicR ~ phantom2*phantom
  UnivEduAchive ~ phantom3*phantom
  

<<<<<<< HEAD
PersonalR ~ C1*GradeF + C2*HregionF
UnivEduAchive ~ c*PersonalR + b*AcademicR +  C3*HregionF + C4*GradeF
AcademicR ~ a*PersonalR + C5*HregionF + C6*GradeF
=======
  PersonalR ~ C1*Grade + C4*Hregion
  AcademicR ~ a*PersonalR + C2*Grade + C5*Hregion
  UnivEduAchive ~ c*PersonalR + b*AcademicR +  C3*Grade + C5*Hregion
>>>>>>> edac05ab7620e409e4c8943fe3b171a8b1a39c12
  
  
  phantom =~ 0
  phantom ~~ 1*phantom'

ptab = lavaanify(model = sem_model , auto = TRUE, model.type="sem")
ptab[c(13,16,17),1:4]

my.sa <- sa.aco(model = sem_model, sens.model = sen_model,sample.cov = full,
                sample.nobs = 230, opt.fun = 3, paths = c(13,16,17), k = 5, max.iter = 1000)

sem_path <-
<<<<<<< HEAD
  'PersonalR ~ C1*GradeF + C2*HregionF
UnivEduAchive ~ c*PersonalR + b*AcademicR +  C3*HregionF + C4*GradeF
AcademicR ~ a*PersonalR + C5*HregionF + C6*GradeF'
=======
  'AcademicR ~ PersonalR
  UnivEduAchive ~ PersonalR + AcademicR'
>>>>>>> edac05ab7620e409e4c8943fe3b171a8b1a39c12


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

