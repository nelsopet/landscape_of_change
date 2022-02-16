#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(readxl)
require(tidyverse)
require(googledrive)
require(utils)


#-----------------------------------------------------------------#
####      Inputing Historical Raw Data from Google Drive       ####
#-----------------------------------------------------------------#

##INSECTS
#Insect data from Proctor 1800s
drive_download((drive_find(pattern = 'proctorinsect_rawdata', n_max=1)), path = 'data/proctorinsect_rawdata_readin.xlsx', overwrite = TRUE)
#lep.his.ALL <- read_excel('data/proctorinsect_rawdata_readin.xlsx', sheet = 1)
#api.his.ALL <- read_excel('data/proctorinsect_rawdata_readin.xlsx', sheet = 2)

#Insect taxonomy
drive_download('https://drive.google.com/file/d/1QbCA8W7ernjGttTB6kH5KG3zYsbqnf4r/view?usp=sharing', path = "data/")
#inat.taxon <- read.csv('data/inat_taxonomy_20220130.csv', header = TRUE)


##BIRDS
#Bird data from 1880s Champlain Society, Spelman, etc.
drive_download((drive_find(pattern = 'csbirds_rawdata', n_max=1)), path = 'data/csbirds_rawdata_readin.xlsx', overwrite = TRUE)
#bird.his.ALL <- read_excel('data/csbirds_rawdata_readin.xlsx')


##LOC Circle
#Download the shapefiles for filtering bird modern bird data
drive_download((drive_find(pattern = 'MDI_Circle.dbf')), path = 'data/MDI_Circle.dbf')
drive_download((drive_find(pattern = 'MDI_Circle.prj')), path = 'data/MDI_Circle.prj')
drive_download((drive_find(pattern = 'MDI_Circle.shp')), path = 'data/MDI_Circle.shp')
drive_download((drive_find(pattern = 'MDI_Circle.shx')), path = 'data/MDI_Circle.shx')
