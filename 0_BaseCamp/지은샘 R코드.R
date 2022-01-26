install.packages("lavaan", dependencies=T)
library(lavaan)

data1 <- read.csv(file.choose())
str(data1)
head(data1)

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


cfa.model_2 <-
  'total_2=~1*cp_1+NA*ep_1+1*bp_1
  cp_1 =~ cp1+cp2+cp3+cp4+cp5
  ep_1 =~ ep1+ep2+ep3+ep4+ep5
  bp_1 =~ bp1+bp2+bp3+bp4+bp5
total_2~~total_2
cp1~~cp2
cp2~~cp3
cp3~~cp4
cp4~~cp5
ep1~~ep2
ep2~~ep3
ep3~~ep4
ep4~~ep5
bp1~~bp2
bp2~~bp3
bp3~~bp4
bp4~~bp5'

cfa.out_2 <- cfa(cfa.model_2, data1)
summary(cfa.out_2, fit.measures=T, standardized=T)
summary(cfa.out_2, fit.measures=T, standardized=T, rsquare=T)

cfa.model_3 <-
  'total_3= KG+ MG + EG
total_3~~total_3
KG~~MG
Mg~~EG'

cfa.out_3 <- cfa(cfa.model_3, data1)
summary(cfa.out_3, fit.measures=T, standardized=T)
summary(cfa.out_3, fit.measures=T, standardized=T, rsquare=T)

install.packages("writexl")
library(writexl)
write_xlsx(Fin,path="data1.xlsx")
write_xlsx(caumed2,path="path1.xlsx")


library(psych)
des<-describe(data1)
View(des)
write_xlsx(des,path=="des.xlsx")
cor.T<-corr.test(cortest)
cor.T$p

alpha(path1)
cronbach(path1)

path1 <- read.csv(file.choose())

library(psych)
des<-describe(path1)
View(des)

cor.test(path1$cog, path1$emo)
cor.test(path1$emo, path1$fin)
cor.test(path1$cog, path1$fin)
cor.test(path1$cog, path1$pa)
cor.test(path1$emo, path1$pa)
cor.test(path1$fin, path1$pa)

cor.test(path1$cp, path1$ep)
cor.test(path1$ep, path1$bp)
cor.test(path1$bp, path1$cp)
cor.test(path1$cp, path1$la)
cor.test(path1$ep, path1$la)
cor.test(path1$bp, path1$la)

cor.test(path1$cog, path1$cp)
cor.test(path1$cog, path1$ep)
cor.test(path1$cog, path1$bp)
cor.test(path1$emo, path1$cp)
cor.test(path1$emo, path1$ep)
cor.test(path1$emo, path1$bp)
cor.test(path1$fin, path1$cp)
cor.test(path1$fin, path1$ep)
cor.test(path1$fin, path1$bp)
cor.test(path1$pa, path1$la)
cor.test(path1$pa, path1$cp)
cor.test(path1$pa, path1$ep)
cor.test(path1$pa, path1$bp)
cor.test(path1$la, path1$cog)
cor.test(path1$la, path1$emo)
cor.test(path1$la, path1$fin)

cor.test(path1$RKG, path1$cp)
cor.test(path1$RKG, path1$ep)
cor.test(path1$RKG, path1$bp)
cor.test(path1$RMG, path1$cp)
cor.test(path1$RMG, path1$ep)
cor.test(path1$RMG, path1$bp)
cor.test(path1$REG2, path1$cp)
cor.test(path1$REG2, path1$ep)
cor.test(path1$REG2, path1$bp)
cor.test(path1$grade1, path1$la)
cor.test(path1$grade1, path1$cp)
cor.test(path1$grade1, path1$ep)
cor.test(path1$grade1, path1$bp)
cor.test(path1$la, path1$RKG)
cor.test(path1$la, path1$RMG)
cor.test(path1$la, path1$REG2)



cor.test(path1$RKG, path1$cog)
cor.test(path1$RKG, path1$emo)
cor.test(path1$RKG, path1$fin)
cor.test(path1$RMG, path1$cog)
cor.test(path1$RMG, path1$emo)
cor.test(path1$RMG, path1$fin)
cor.test(path1$REG2, path1$cog)
cor.test(path1$REG2, path1$emo)
cor.test(path1$REG2, path1$fin)
cor.test(path1$grade2, path1$pa)
cor.test(path1$grade2, path1$cog)
cor.test(path1$grade2, path1$emo)
cor.test(path1$grade2, path1$fin)
cor.test(path1$pa, path1$RKG)
cor.test(path1$pa, path1$RMG)
cor.test(path1$pa, path1$REG2)





model4.cfa <- 
  'pa =~ cog+emo+fin
   la =~ cp+ep+bp'
out.cfa <- cfa(model4.cfa, path1)
summary(out.cfa, fit.measures=T, standardized=T)

sr.model <- 
  'Pa_5 =~ cog+emo+fin
   La_5 =~ cp+ep+bp
   Grade_5 =~ RKG+RMG+REG1
   Grade_5 ~ Pa_5+La_5
   La_5~Pa_5'
   
sr.out <- sem(sr.model, path1)
summary(sr.out, fit.measures=T, standardized=T)
summary(sr.out, fit.measures=T, standardized=T, rsquare=T)



sr.model <- 
  'Pa_ =~ cog_3+emo_3+fin_3
   La_3 =~ cp_3+ep_3+bp_3
   Grade_3 =~KG_3+MG_3+EG_3
   Grade_3 ~ Pa_3+La_3
   La_3~Pa_3'

sr.out <- sem(sr.model, path1)
summary(sr.out, fit.measures=T, standardized=T)
summary(sr.out, fit.measures=T, standardized=T, rsquare=T)










sr.model.re <- 
  'pa =~ cog+emo+fin
   la =~ cp+ep+bp
   grade1 ~ pa+la
   la~pa'
sr.out.re <- sem(sr.model.re, path1)

anova(sr.out, sr.out.re)
summary(sr.out.re, fit.measures=T, standardized=T)

sr.model.med <-
  'pa3 =~ cog+emo+fin
   la3 =~ cp+ep+bp
   grade3 ~ c*la3
   la3 ~ b*pa
   med:=b*c'

set.seed(123)
path.out.med<-sem(path.model.med, path1, se="bootstrap", bootstrap=1000)
parameterestimates(path.out.med)

mg.model1<-
  'pa =~ cog+emo+fin
   la =~ cp+ep+bp
   grade ~ pa+la
   la ~ pa'
   
mg.out1<-sem(mg.model1, path1, group="Congr", meanstructure=F)
summary(mg.out1, fit.measures=T)

library(moments)
library(lavaan)
library(psych)


skewness(Fin[20])
skewness(Fin[2:16])
kurtosis(Fin[2:16])
kurtosis(Fin[20]

         
path1.model <- 
  'lifesat_m ~ friend_m + selfest_m
   selfest_m ~ friend_m'
  path.out <- sem(path.model, data1)
  summary(path.out, fit.measures=T, standardized=T)
  
  
path1.model.med <- 
'grade_4 ~ a*pa_4 + c*la_4
 grade_4 ~ b*pa_4
 med := b*c
 total := a+b*c'
  
set.seed(123)
path.path1.med <- sem(path1.model.med, path1, se="bootstrap", bootstrap=1000)
summary(path.path1.med,fit.measures=T, standardized=T)  
parameterestimates(path.path1.med)
  
         
path1 <- read.csv("path1.csv")
head(path1)

model1 <- 
  'grade1 ~ a*pa + c*la
   la ~ b*pa
   med:= b*c
   total:= a+b*c'

set.seed(123)
path.out1 <- sem(path.model1, path1)
summary(out1, fit.measures=T, standardized=T)
out1.boot <- sem(model1, path1, se="bootstrap", bootstrap=1000)
parameterestimates(out1.boot)


path.model.med <-
  'grade1 ~ a*pa + c*la
   la ~ b*pa
   med:= b*c
   total:= a+b*c'

set.seed(123)
path.out.med <- sem(path.model.med, path1, se="bootstrap", bootstrap=1000)
parameterestimates(path.out.med)
summary(path.out.med,fit.measures=T, standardized=T)

path.model<-
  'grade ~ pa + la
   la ~ pa'
path.out <-sem(path.model, path1)

path.model.re <-
  'grade ~ la
   la ~ pa'
path.out.re <-sem(path.model.re, path1)
summary(path.out.re, fit.measures=T, standardized=T)

anova(path.out, path.out.re)

sr.model.med <-
  'pa3 =~ cog+emo+fin
   la3 =~ cp+ep+bp
   grade3 ~ c*la3
   la3 ~ b*pa
   med:=b*c'

set.seed(123)
path.out.med<-sem(path.model.med, path1, se="bootstrap", bootstrap=1000)
parameterestimates(path.out.med)

         