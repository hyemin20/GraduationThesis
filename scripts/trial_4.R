

set.seed(111)
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
  UnivEduAchive ~ c1*PersonalR + b1*AcademicR + b2*gender + b3*Personalgender 

  first_F := a1 + a3*1
  first_M := a1 + a3*2
  
  middle_F := b1 + b3*1
  middle_M := b1 + b3*2
  
  indirect_F := first_F*middle_F
  indirect_M := first_M*middle_M
  
  direct_F := c1 
  direct_M := c1 
  
  
  total_F := indirect_F + direct_F
  total_M := indirect_M + direct_M'

set.seed(111)
fit.mod.med_1 <- sem(model=mod.model_1, data=df_na_b.mod.med_1, se="bootstrap", bootstrap=10)
summary(fit.mod.med_1, standardized=TRUE)







set.seed(111)
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

set.seed(111)
fit.mod.med_2 <- sem(model=mod.model_2, data=df_na_b.mod.med_2, se="bootstrap", bootstrap=10)
summary(fit.mod.med_2, standardized=TRUE)



