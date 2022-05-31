rm(list=ls())

install.packages('readxl')
install.packages('writexl')
install.packages('dplyr') 
library(dplyr)
library(readxl)
library(writexl)

First <- read_excel("C:/Users/OWNER/Downloads/first.xlsx",  col_names = TRUE)
Second <- read_excel("C:/Users/OWNER/Downloads/second.xlsx",  col_names = TRUE)
Third <- read_excel("C:/Users/OWNER/Downloads/third.xlsx",  col_names = TRUE)


FirstSecond <- full_join(First,Second,by='ID')
Full <-  full_join(FirstSecond, Third, by='ID')

write_xlsx(Full, path = "/Users/OWNER/Downloads/full_join.xlsx")

install.packages('proccessmap')
lib = 
.libPaths("C:/Program Files (x86)/R/R-4.1.2/library")

### 1.19

join <- read_excel("D:/HYEM'S/GraduatedSchool/GraduationThesis/full_join.xlsx",  col_names = TRUE)
GPA <- read_excel("D:/HYEM'S/GraduatedSchool/GraduationThesis/GPA.xlsx",  col_names = TRUE)
univ <- read_excel("D:/HYEM'S/GraduatedSchool/GraduationThesis/ID_univ.xlsx",  col_names = TRUE)

Full2 <-  full_join(univ, GPA, by='ID')
write_xlsx(Full2, path = "D:/HYEM'S/GraduatedSchool/GraduationThesis/FULL2.xlsx")
  