#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(utils)
require(readxl)
require(tidyverse)


#------------------------------------------------#
####     Grabbing dataframes for analysis     ####
#------------------------------------------------#

#Bring in the processed cs bird data from Google Drive
drive_download((drive_find(pattern = 'proctorinsect_processed', n_max=1)), path = 'data/proctorinsect_processed_readin.csv', overwrite = TRUE)
insect.his.analysis <- read.csv('data/proctorinsect_processed_readin.csv', header = TRUE)

#Bring in the processed ebird data from Google Drive
drive_download((drive_find(pattern = 'inatinsect_processed', n_max=1)), path = 'data/inatinsect_processed_readin.csv', overwrite = TRUE)
insect.mod.analysis <- read.csv('data/inatinsect_processed_readin.csv', header = TRUE)


#------------------------------------------------#
####   Analysis and Creating visualizations   ####
#------------------------------------------------#









