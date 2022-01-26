###' ###########################################################################
###' 
###' Project(project name): Graduation Thesis
###' 
###' Category(stage in the project): Data management
###' 
###' Task(specific task in the category): Import raw datasets
###' 
###' Data(data source): `survey data`
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
library(tidyverse)
library(RSQLite)
library(lubridate)


### Call functions
list.files(file.path(work_dir, "functions"), full.names = TRUE) %>% walk(source)

