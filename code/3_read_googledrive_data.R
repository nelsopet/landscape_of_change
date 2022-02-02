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

#First we need to create a function that returns the file name of the most recent version 
#of the historical bird and insect data sets

#Automated newest file name output for Champlain Society bird data
bird.his.newest <- function(x) {
  bird <- basename(list.files(pattern = 'csbirds_rawdata'))
  return(bird[1])
}

#Automated newest file name output for Proctor's insect bird data
insect.his.newest <- function(x) {
  insect <- basename(list.files(pattern = 'proctorinsect_rawdata')) 
  return(insect[1])
}


#Change WD to put download data into the 'data' folder
setwd(paste0(getwd(), "/data"))

##INSECTS
#Insect data from Proctor 1800s
drive_download((drive_find(pattern = 'proctorinsect_rawdata', n_max=1)), overwrite = TRUE)
lep.his.ALL <- read_excel(insect.his.newest(insect), sheet = 1)
api.his.ALL <- read_excel(insect.his.newest(insect), sheet = 2)

#Insect taxonomy
drive_download('https://drive.google.com/file/d/1QbCA8W7ernjGttTB6kH5KG3zYsbqnf4r/view?usp=sharing')
inat.taxon <- read.csv('inat_taxonomy_20220130.csv', header = TRUE)


##BIRDS
#Bird data from 1880s Champlain Society, Spelman, etc.
drive_download((drive_find(pattern = 'csbirds_rawdata', n_max=1)), overwrite = TRUE)
bird.his.ALL <- read_excel(bird.his.newest(bird))

#Return working directory to main folder
wd <- getwd()
setwd(gsub("/data", "", wd))

