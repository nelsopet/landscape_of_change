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
drive_download((drive_find(pattern = 'csbirds_processed', n_max=1)), path = 'data/csbirds_processed_readin.csv', overwrite = TRUE)
bird.his.analysis <- read.csv('data/csbirds_processed_readin.csv', header = TRUE)

#Bring in the processed ebird data from Google Drive
drive_download((drive_find(pattern = 'ebird_processed', n_max=1)), path = 'data/ebird_processed_readin.csv', overwrite = TRUE)
bird.mod.analysis <- read.csv('data/ebird_processed_readin.csv', header = TRUE)


#------------------------------------------------#
####   Analysis and Creating visualizations   ####
#------------------------------------------------#








