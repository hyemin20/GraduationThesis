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

length(unique(df_na$ID))
length(unique(df_na_a$ID))
length(unique(df_na_b$ID))


stu_id <- unique(df_na$ID)
stu_id_a <- unique(df_na_a$ID)
setdiff(stu_id_a, stu_id)

df_na_a <- df_na_a %>%
  filter(ID %in% stu_id)

df_na_b <- df_na_b %>%
  filter(ID %in% stu_id)




nrow(df_na_aa)
nrow(df_na_bb)

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



###' ###########################################################################'
###' 
###' Import survey items
###' 
###' 

#### Check variable

lapply(df_na_a, class)
lapply(df_na_b, class)


map(df_indepen_n,summary)
lapply(df_indepen_n, sd)

map(df_medi,summary)
lapply(df_medi, sd)

map(df_depen_1,summary)
lapply(df_depen_1, sd)

map(df_depen_2,summary)
lapply(df_depen_2, sd)




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

skewness(df_indepen_n$R_total)
kurtosis(df_indepen_n$R_total)
shapiro.test(df_indepen_n$R_total)
ks.test(df_indepen_n$R_total, "pnorm", mean= mean(df_indepen_n$R_total), sd = sd(df_indepen_n$R_friend))



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

skewness(df_medi$Resili_total)
kurtosis(df_medi$Resili_total)
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

skewness(df_depen_1$UnivEduSatis_total)
kurtosis(df_depen_1$UnivEduSatis_total)
shapiro.test(df_depen_1$UnivEduSatis_total)
ks.test(df_depen_1$UnivEduSatis_total, "pnorm")



### GPA
a <- ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA))
b <- ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA_new))
c <- ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA_5))
grid.arrange(a,b,c, nrow=1, ncol=3)

skewness(df_depen_2$GPA)
kurtosis(df_depen_2$GPA)
shapiro.test(df_depen_2$GPA)
ks.test(df_depen_2$GPA, "pnorm", mean= mean(df_depen_2$GPA), sd = sd(df_depen_2$GPA))


#for negatively skewed data
#df_depen_2$GPA_a <- sqrt(max(df_depen_2$GPA+1) - df_depen_2$GPA)
#df_depen_2$GPA_b <- log10(max(df_depen_2$GPA+1) - df_depen_2$GPA)

df_depen_2$GPA_new <- 1/(max(df_depen_2$GPA+1) - df_depen_2$GPA)
df_na_a$GPA_new <- 1/(max(df_depen_2$GPA+1) - df_depen_2$GPA)
df_na_b$GPA_new <- 1/(max(df_depen_2$GPA+1) - df_depen_2$GPA)

df_depen_2$GPA_5 <- 1/(max(df_depen_2$GPA_scaled_5+1) - df_depen_2$GPA_scaled_5)
df_na_a$GPA_5 <- 1/(max(df_depen_2$GPA_scaled_5+1) - df_depen_2$GPA_scaled_5)
df_na_b$GPA_5 <- 1/(max(df_depen_2$GPA_scaled_5+1) - df_depen_2$GPA_scaled_5)

df_depen_2$GPA_5_scaled <- df_depen_2$GPA_5*5
df_na_a$GPA_5_scaled <- df_na_a$GPA_5*5
df_na_b$GPA_5_scaled <- df_na_b$GPA_5*5


df_na_a <- df_na_a %>%
  relocate(matches("^GPA"), .after = GPA)
df_na_b <- df_na_b %>%
  relocate(matches("^GPA"), .after = GPA)


ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA_a))
ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA_b))
b <- ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA_new))
c <- ggplot(data = df_depen_2) + geom_density(mapping=aes(x=GPA_5_scaled))



grid.arrange(a,b,c, nrow=1, ncol=3)



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
cor(df_indepen_n, df_medi)

cor(df_indepen_n, df_depen_1)
cor(df_indepen_n, df_depen_2)

cor(df_medi, df_depen_1)
cor(df_medi, df_depen_2)


### cor test indepen-medi
cor.test(df_indepen_n$R_friend, df_medi$Resili_SelfEffi)
cor.test(df_indepen_n$R_friend, df_medi$Resili_SituJuge)
cor.test(df_indepen_n$R_friend, df_medi$Resili_Resource)
cor.test(df_indepen_n$R_friend, df_medi$Resili_Vital)
cor.test(df_indepen_n$R_friend, df_medi$Resili_FutureOrien)
cor.test(df_indepen_n$R_friend, df_medi$Resili_total)

cor.test(df_indepen_n$R_prof, df_medi$Resili_SelfEffi)
cor.test(df_indepen_n$R_prof, df_medi$Resili_SituJuge)
cor.test(df_indepen_n$R_prof, df_medi$Resili_Resource)
cor.test(df_indepen_n$R_prof, df_medi$Resili_Vital)
cor.test(df_indepen_n$R_prof, df_medi$Resili_FutureOrien)
cor.test(df_indepen_n$R_prof, df_medi$Resili_total)

cor.test(df_indepen_n$R_parent, df_medi$Resili_SelfEffi)
cor.test(df_indepen_n$R_parent, df_medi$Resili_SituJuge)
cor.test(df_indepen_n$R_parent, df_medi$Resili_Resource)
cor.test(df_indepen_n$R_parent, df_medi$Resili_Vital)
cor.test(df_indepen_n$R_parent, df_medi$Resili_FutureOrien)
cor.test(df_indepen_n$R_parent, df_medi$Resili_total)

cor.test(df_indepen_n$R_total, df_medi$Resili_SelfEffi)
cor.test(df_indepen_n$R_total, df_medi$Resili_SituJuge)
cor.test(df_indepen_n$R_total, df_medi$Resili_Resource)
cor.test(df_indepen_n$R_total, df_medi$Resili_Vital)
cor.test(df_indepen_n$R_total, df_medi$Resili_FutureOrien)
cor.test(df_indepen_n$R_total, df_medi$Resili_total)



### cor test indepen-depen
cor.test(df_depen_1$UnivEduSatis_liberalarts, df_indepen_n$R_friend)
cor.test(df_depen_1$UnivEduSatis_liberalarts, df_indepen_n$R_prof)
cor.test(df_depen_1$UnivEduSatis_liberalarts, df_indepen_n$R_parent)
cor.test(df_depen_1$UnivEduSatis_liberalarts, df_indepen_n$R_total)

cor.test(df_depen_1$UnivEduSatis_major, df_indepen_n$R_friend)
cor.test(df_depen_1$UnivEduSatis_major, df_indepen_n$R_prof)
cor.test(df_depen_1$UnivEduSatis_major, df_indepen_n$R_parent)
cor.test(df_depen_1$UnivEduSatis_major, df_indepen_n$R_total)

cor.test(df_depen_1$UnivEduSatis_infra, df_indepen_n$R_friend)
cor.test(df_depen_1$UnivEduSatis_infra, df_indepen_n$R_prof)
cor.test(df_depen_1$UnivEduSatis_infra, df_indepen_n$R_parent)
cor.test(df_depen_1$UnivEduSatis_infra, df_indepen_n$R_total)

cor.test(df_depen_1$UnivEduSatis_total, df_indepen_n$R_friend)
cor.test(df_depen_1$UnivEduSatis_total, df_indepen_n$R_prof)
cor.test(df_depen_1$UnivEduSatis_total, df_indepen_n$R_parent)
cor.test(df_depen_1$UnivEduSatis_total, df_indepen_n$R_total)

cor.test(df_depen_2$GPA, df_indepen_n$R_friend)
cor.test(df_depen_2$GPA, df_indepen_n$R_prof)
cor.test(df_depen_2$GPA, df_indepen_n$R_parent)
cor.test(df_depen_2$GPA, df_indepen_n$R_total)

cor.test(df_depen_2$GPA_new, df_indepen_n$R_friend)
cor.test(df_depen_2$GPA_new, df_indepen_n$R_prof)
cor.test(df_depen_2$GPA_new, df_indepen_n$R_parent)
cor.test(df_depen_2$GPA_new, df_indepen_n$R_total)

cor.test(df_depen_2$GPA_5_scaled, df_indepen_n$R_friend)
cor.test(df_depen_2$GPA_5_scaled, df_indepen_n$R_prof)
cor.test(df_depen_2$GPA_5_scaled, df_indepen_n$R_parent)
cor.test(df_depen_2$GPA_5_scaled, df_indepen_n$R_total)


### cor test medi-depen
cor.test(df_depen_1$UnivEduSatis_liberalarts, df_medi$Resili_SelfEffi)
cor.test(df_depen_1$UnivEduSatis_liberalarts, df_medi$Resili_SituJuge)
cor.test(df_depen_1$UnivEduSatis_liberalarts, df_medi$Resili_Resource)
cor.test(df_depen_1$UnivEduSatis_liberalarts, df_medi$Resili_Vital)
cor.test(df_depen_1$UnivEduSatis_liberalarts, df_medi$Resili_FutureOrien)
cor.test(df_depen_1$UnivEduSatis_liberalarts, df_medi$Resili_total)

cor.test(df_depen_1$UnivEduSatis_major, df_medi$Resili_SelfEffi)
cor.test(df_depen_1$UnivEduSatis_major, df_medi$Resili_SituJuge)
cor.test(df_depen_1$UnivEduSatis_major, df_medi$Resili_Resource)
cor.test(df_depen_1$UnivEduSatis_major, df_medi$Resili_Vital)
cor.test(df_depen_1$UnivEduSatis_major, df_medi$Resili_FutureOrien)
cor.test(df_depen_1$UnivEduSatis_major, df_medi$Resili_total)

cor.test(df_depen_1$UnivEduSatis_infra, df_medi$Resili_SelfEffi)
cor.test(df_depen_1$UnivEduSatis_infra, df_medi$Resili_SituJuge)
cor.test(df_depen_1$UnivEduSatis_infra, df_medi$Resili_Resource)
cor.test(df_depen_1$UnivEduSatis_infra, df_medi$Resili_Vital)
cor.test(df_depen_1$UnivEduSatis_infra, df_medi$Resili_FutureOrien)
cor.test(df_depen_1$UnivEduSatis_infra, df_medi$Resili_total)

cor.test(df_depen_1$UnivEduSatis_total, df_medi$Resili_SelfEffi)
cor.test(df_depen_1$UnivEduSatis_total, df_medi$Resili_SituJuge)
cor.test(df_depen_1$UnivEduSatis_total, df_medi$Resili_Resource)
cor.test(df_depen_1$UnivEduSatis_total, df_medi$Resili_Vital)
cor.test(df_depen_1$UnivEduSatis_total, df_medi$Resili_FutureOrien)
cor.test(df_depen_1$UnivEduSatis_total, df_medi$Resili_total)

cor.test(df_depen_2$GPA, df_medi$Resili_SelfEffi)
cor.test(df_depen_2$GPA, df_medi$Resili_SituJuge)
cor.test(df_depen_2$GPA, df_medi$Resili_Resource)
cor.test(df_depen_2$GPA, df_medi$Resili_Vital)
cor.test(df_depen_2$GPA, df_medi$Resili_FutureOrien)
cor.test(df_depen_2$GPA, df_medi$Resili_total)

cor.test(df_depen_2$GPA_new, df_medi$Resili_SelfEffi)
cor.test(df_depen_2$GPA_new, df_medi$Resili_SituJuge)
cor.test(df_depen_2$GPA_new, df_medi$Resili_Resource)
cor.test(df_depen_2$GPA_new, df_medi$Resili_Vital)
cor.test(df_depen_2$GPA_new, df_medi$Resili_FutureOrien)
cor.test(df_depen_2$GPA_new, df_medi$Resili_total)

cor.test(df_depen_2$GPA_5_scaled, df_medi$Resili_SelfEffi)
cor.test(df_depen_2$GPA_5_scaled, df_medi$Resili_SituJuge)
cor.test(df_depen_2$GPA_5_scaled, df_medi$Resili_Resource)
cor.test(df_depen_2$GPA_5_scaled, df_medi$Resili_Vital)
cor.test(df_depen_2$GPA_5_scaled, df_medi$Resili_FutureOrien)
cor.test(df_depen_2$GPA_5_scaled, df_medi$Resili_total)



###' ###########################################################################'
###' 
###' CFA, cronbach's alpha
###' 
###'


df_cron <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/GraduationThesis/datasets/01_coding_05_full_1234_A.csv")
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
  R_prof =~ R_prof_famil + R_prof_credit + R_prof_respect
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
   UnivEduSatis_liberalarts =~ UnivEduSatis_liberalarts_contents + UnivEduSatis_liberalarts_method + UnivEduSatis_liberalarts_assess + UnivEduSatis_liberalarts_prof
   UnivEduSatis_major =~ UnivEduSatis_major_contents + UnivEduSatis_major_method + UnivEduSatis_major_assess + UnivEduSatis_major_prof
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


#####################################################################################
#####################################################################################
sem_model_5_2 <-
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
  UnivEduAchive ~ PersonalR + AcademicR
  AcademicR ~ PersonalR'

out_5_2 <- sem(sem_model_5_2, df_na_b)
fitMeasures(out_5_2 ,c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr"))

lavInspect(out_5_2,"theta")
inspect(out_5_2, "cov.lv")


standardizedsolution(out_5_2) %>%
  filter(op %in% c("=~","~")) %>%
  mutate(stars = ifelse(pvalue<0.001, "***", ifelse(pvalue<0.01, "**", ifelse(pvalue<0.05, "*", ""))))

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


#####################################################################################
#####################################################################################
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
sem.med <- 
  # measurement model
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
  
  # regressions
  AcademicR ~ a*PersonalR
  UnivEduAchive ~ c*PersonalR + b*AcademicR

  # indirect effect: ab
  ab := a*b
  # total effect: c
  d := c + (a*b)'


set.seed(123)
fit.med <- sem(model=sem.med, data=df_na_b, se="bootstrap", bootstrap=10)
summary(fit.med, standardized=TRUE)


AparameterEstimates(fit.med, standardized=TRUE) %>% 
  filter(op == "~" | op == ":=") %>% 
  mutate(stars=ifelse(pvalue < 0.001, "***", 
                      ifelse(pvalue < 0.01, "**", 
                             ifelse(pvalue < 0.05, "*", ""))))

semPaths(fit.med, what="std", layout="tree", edge.label.cex=1, 
         edge.color="royalblue", 
         color=list(lat="tomato", man="grey"), fade=FALSE, 
         style="lisrel", curvature=2, title = TRUE,
         rotation = 2)

a = 0.213 ; b = 0.740
s_a = 0.057 ;  s_b = 0.248


a = 0.180 ; b = 0.848
s_a = 0.043 ;  s_b = 0.217
z = a * b / sqrt(b^2 * s_a^2 + a^2 * s_b^2)
z

2 * pnorm(z, lower.tail = F)



### Mediation Effect Analysis_2
names(df_na_b)
table(df_na_b$Depart)
sem.med <- 
  # measurement model
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
  
  # regressions
  AcademicR ~ a*PersonalR + C1*Residence + C2*Hregion + C3*Htype + C4*Grade + C5*Depart
  UnivEduAchive ~ c*PersonalR + b*AcademicR + C6*Residence + C7*Hregion + C8*Htype + C9*Grade + C10*Depart

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

a = 0.217 ; b =  0.696
s_a = 0.058 ;  s_b = 0.240
z = a * b / sqrt(b^2 * s_a^2 + a^2 * s_b^2)
z

2 * pnorm(z, lower.tail = F)



A###' ###########################################################################'
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

nrow(df_na_a)


### sensitive analysis_1
sem_model <- 
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
  
  AcademicR ~ a*PersonalR + C1*Residence + C2*Hregion + C3*Htype + C4*Grade + C5*Depart
  UnivEduAchive ~ c*PersonalR + b*AcademicR + C6*Residence + C7*Hregion + C8*Htype + C9*Grade + C10*Depart'

sen_model <- 
  'PersonalR =~ R_friend + R_prof + R_parent
  AcademicR =~ Resili_SelfEffi + Resili_SituJuge + Resili_Resource + Resili_Vital + Resili_FutureOrien
  UnivEduAchive =~ UnivEduSatis_total_a + GPA_5_scaled
    
  PersonalR ~ phantom1*phantom
  AcademicR ~ phantom2*phantom
  
  AcademicR ~ a*PersonalR + C1*Residence + C2*Hregion + C3*Htype + C4*Grade + C5*Depart
  UnivEduAchive ~ c*PersonalR + b*AcademicR + C6*Residence + C7*Hregion + C8*Htype + C9*Grade + C10*Depart
  
  phantom =~ 0
  phantom ~~ 1*phantom'

sem_path <-
  '  AcademicR ~ a*PersonalR
  UnivEduAchive ~ c*PersonalR + b*AcademicR'


sens_analysis <- sa.aco(data = df_na_b,
                        sample.cov = full,
                        sample.nobs = 230,
                        model = sem_model,
                        opt.fun = 3,
                        rate.of.conv = 0.001,
                        sens.model = sen_model,
                        paths = sem_path)

sens_tables <- sens.tables(sens_analysis)
sens_tables$sens.summary
sens_tables$phan.paths

