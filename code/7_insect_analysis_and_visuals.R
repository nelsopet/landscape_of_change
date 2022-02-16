#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(utils)
require(tidyverse)
require(ggplot2)
require(dplyr)
require(googledrive)

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

###Create a count of species in each family
##Historical
#Create the count column
his.freq <- as.data.frame(table(insect.his.analysis$family)) %>% 
  rename('family'='Var1')

#Gather taxonomic columns for merge
his.simp <- insect.his.analysis %>% 
  dplyr::select('order','super.family','family')

#Merge with the count data
his.count.ALL <- merge(his.simp, his.freq, by = 'family')

#Clean a final family list with total species for each
his.count <- his.count.ALL[!duplicated(his.count.ALL$family), ] 


##Modern
#Create the count column
mod.freq <- as.data.frame(table(insect.mod.analysis$family)) %>% 
  rename('family'='Var1')

#Gather taxonomic columns for merge
mod.simp <- insect.mod.analysis %>% 
  dplyr::select('order','super.family','family')

#Merge with the count data
mod.count.ALL <- merge(mod.simp, mod.freq, by = 'family')

#Clean a final family list with total species for each
mod.count <- mod.count.ALL[!duplicated(mod.count.ALL$family), ] 


##Merge the historic and modern data for comparison
insect.counts <- left_join(his.count, mod.count, by = 'family')

insect.counts2 <- insect.counts %>% 
  dplyr::select('order.x','super.family.x','family','Freq.x','Freq.y') %>% 
  rename('order'='order.x','super.family'='super.family.x','frequency.1900s'='Freq.x','frequency.current'='Freq.y')

insect.counts2[is.na(insect.counts2)] <- 0



